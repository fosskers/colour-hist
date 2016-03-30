{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Module    : Data.Colour.Histogram
-- Copyright : (c) Colin Woodbury, 2016
-- License   : GPL-3
-- Maintainer: Colin Woodbury <colingw@gmail.com>
--
-- A *Colour Histogram*, as defined in the 1991 paper *Colour Indexing* by
-- Swain and Ballard:
-- http://www.inf.ed.ac.uk/teaching/courses/av/LECTURE_NOTES/swainballard91.pdf
--
-- Colour Histograms and their associated algorithms can be used to:
--
--   1. Find a known object in some location within an image
--
--   2. Identify an object in a known location in an image
--
-- This library provides two algorithms for object identification
-- (Histogram Intersection and Incremental Intersection) and one for
-- finding location (Histogram Backprojection).

module Data.Colour.Histogram
       (
         -- * Histograms
         -- ** Types
         AsHistogram(..)
       , Histogram(..)
       , YCbCrHist
       , RGBHist
         -- ** Creation
       , hist
       , ycbcrHist
       , rgbHist
       , rgbConstancy
         -- * Indices
         -- ** Types
       , CbCr(..)
       , RG(..)
         -- ** Bins
       , bin8x8
       , bin16x16
       , bin32x32
       , bin64x64
       , bin128x128
       , bin256x256
       , bin8x8f
       , bin16x16f
       ) where

import qualified Data.HashMap.Strict as HM
import           Data.Hashable
import           Data.Key
import           Data.Ratio
import qualified Data.Vector.Storable as V
import           Data.Word
import           GHC.Generics (Generic)

---

-- | Any datatype which represents a Histogram.
class AsHistogram t where
  -- | The number of pixels in the histogram.
  pixelCount :: t -> Int

  -- | The total number of common pixels found in corresponding bins
  -- between a Model and Image histogram.
  commonPixels :: t -> t -> Int

  -- | Intersection normalized by the number of pixels in the given model's
  -- `Histogram`. `Ratio` is used to prioritise accuracy.
  intersection :: t -> t -> Ratio Int
  intersection t m = commonPixels t m % pixelCount m

-- | An efficient Histogram, implemented internally as a `HashMap` to
-- only store bins which count 1 or more pixels.
newtype Histogram k = Histogram { _hm :: HM.HashMap k Int } deriving (Eq,Show)

instance (Eq k, Hashable k) => AsHistogram (Histogram k) where
  pixelCount = sum . _hm

  commonPixels t m = sum $ zipWithKey (\_ t' m' -> min t' m') (_hm t) (_hm m)

-- | An index type for a two-axis Histogram counting pixels in YCbCr space.
-- By working in YCbCr, our algorithm should perform well regardless of
-- lighting conditions, provided that model and image histograms share
-- a light source.
--
-- If light sources differ, then the CbCr space will be skewed toward
-- Blue for outdoor light, and toward Yellow for indoor light.
--
-- Experiments have yet to be done to show the difference in accuracy
-- between cases.
newtype CbCr = CbCr { _cbcr :: (Word8,Word8) } deriving (Eq,Show,Generic)

instance Hashable CbCr

-- | An index type for a two-axis Histogram counting pixels in an RGB image.
-- A simple colour constancy algorithm is applied to the raw RGB to
-- factor out image intensity, as well as reduce the Histogram to only
-- two axes:
--
-- r' = r/(r+g+b)
--
-- g' = g/(r+g+b)
--
-- b' = b/(r+g+b)
--
-- b' is unneeded, as it can be derived via r' and g'.
newtype RG = RG { _rg :: (Word8,Word8) } deriving (Eq,Show,Generic)

instance Hashable RG

type YCbCrHist = Histogram CbCr

type RGBHist = Histogram RG

-- | Generic function for creating a `Histogram` with any `Hashable` key type.
-- Must provide a function which performs colour constancy and yields
-- an index representing a "bin" in the histogram.
hist :: (Eq k, Hashable k) => ((Word8,Word8,Word8) -> k) -> V.Vector Word8 -> Histogram k
hist f v = Histogram $ go v HM.empty
  where go v' hm | V.null v' = hm
                 | otherwise = go v'' $ HM.insertWith (+) k 1 hm
          where k = f (pix V.! 0, pix V.! 1, pix V.! 2)
                (pix,v'') = V.splitAt 3 v'

-- | Create a YCbCr `Histogram`. The input vector is assumed to be a
-- `Data.Vector.Storable` representing an image whose pixels are held
-- in order of their channel, i.e. @[y,cb,cr,y,cb,cr,...]@
-- This is the data format used by the JuicyPixels library.
--
-- The function is a transformation of indices in `Word8` space (i.e. [0-255])
-- to those in a reduced "bin" space.
--
-- Example:
--
-- > ycbcrHist bin8x8 imageVec
ycbcrHist :: ((Word8,Word8) -> (Word8,Word8)) -> V.Vector Word8 -> YCbCrHist
ycbcrHist f = hist (\(_,cb,cr) -> CbCr $ f (cb,cr))

-- | Analogous to `ycbcrHist`. Example:
--
-- > rgbHist (bin8x8 . rgbConstancy) imageVec
rgbHist :: ((Word8,Word8,Word8) -> (Word8,Word8)) -> V.Vector Word8 -> RGBHist
rgbHist f = hist (RG . f)

-- | Account for lighting differences between RGB images.
-- This reduces the original RGB triplet to a modified RG pair, as
-- described in the `RG` index type.
rgbConstancy :: (Word8,Word8,Word8) -> (Float,Float)
rgbConstancy (fi -> r, fi -> g, fi -> b) = (r / rgb, g / rgb)
  where rgb = r + g + b

scale :: Int -> Word8 -> Word8
scale dim 255 = fi dim - 1
scale dim n = fi $ (dim * fi n) `div` 255

scaleF :: Float -> Word8
scaleF n = round $ n * 765

-- | The @binNxN@ series of functions tranform colour values in the range
-- [0-255] into indices for "bins" of pixels counts.
-- For instance, if you wish to form a `Histogram` with 8 bins per axis,
-- (thus 32 pixels per bin) you would use `bin8x8`.
--
-- Typically, `bin8x8` or `bin16x16` are sufficient.
-- (TODO: need proof)
bin8x8 :: (Word8,Word8) -> (Word8,Word8)
bin8x8 (x,y) = (scale 8 x, scale 8 y)

-- | Analogous to `bin8x8`, but operates on `Float` in the range [0,1/3].
--
-- Note: This will _not_ produce the correct result for normal RGB values
-- normalized to [0,1].
bin8x8f :: (Float,Float) -> (Word8,Word8)
bin8x8f (x,y) = bin8x8 (scaleF x, scaleF y)

-- | 16 pixels per bin.
bin16x16 :: (Word8,Word8) -> (Word8,Word8)
bin16x16 (x,y) = (scale 16 x, scale 16 y)

bin16x16f :: (Float,Float) -> (Word8,Word8)
bin16x16f (x,y) = bin16x16 (scaleF x, scaleF y)

-- | 8 pixels per bin.
bin32x32 :: (Word8,Word8) -> (Word8,Word8)
bin32x32 (x,y) = (scale 32 x, scale 32 y)

-- | 4 pixels per bin.
bin64x64 :: (Word8,Word8) -> (Word8,Word8)
bin64x64 (x,y) = (scale 64 x, scale 64 y)

-- | 2 pixels per bin.
bin128x128 :: (Word8,Word8) -> (Word8,Word8)
bin128x128 (x,y) = (scale 128 x, scale 128 y)

-- | 1 pixel per bin.
bin256x256 :: (Word8,Word8) -> (Word8,Word8)
bin256x256 (x,y) = (scale 256 x, scale 256 y)

fi :: (Integral a, Num b) => a -> b
fi = fromIntegral
{-# INLINE fi #-}
