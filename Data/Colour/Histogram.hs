-- |
-- Module    : Data.Colour.Histogram
-- Copyright : (c) Colin Woodbury, 2016
-- License   : GPL-3
-- Maintainer: Colin Woodbury <colingw@gmail.com>
--
-- A *Colour Histogram*, as defined in the 1991 paper *Colour Indexing* by
-- Swain and Ballard: URL HERE
--
-- Colour Histograms and their associated algorithms can be used to:
--   1. Find a known object in some location within an image
--   2. Identify an object in a known location in an image
--
-- This library provides two algorithms for object identification
-- (Histogram Intersection and Incremental Intersection) and one for
-- finding location (Histogram Backprojection).

module Data.Colour.Histogram
       (
         -- * Histograms
         AsHistogram(..)
       , Histogram(..)
       , YCbCrHist
       , RGBHist
         -- * Index types
       , CbCr(..)
       , RGB(..)
       ) where

import Data.HashMap.Strict
import Data.Key
import Data.Ratio
--import Data.Vector
import Data.Hashable

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
  intersection i m = commonPixels i m % pixelCount m

-- | An efficient Histogram, implemented internally as a `HashMap` to
-- only store bins which count 1 or more pixels.
newtype Histogram k = Histogram { _hm :: HashMap k Int } deriving (Eq,Show)

instance (Eq k, Hashable k) => AsHistogram (Histogram k) where
  pixelCount = sum . _hm

  commonPixels i m = sum $ zipWithKey (\_ i' m' -> min i' m') (_hm i) (_hm m)

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
newtype CbCr = CbCr { _cbcr :: (Int,Int) } deriving (Eq,Show)

-- | An index type for a three-axis Histogram counting pixels in RGB.
newtype RGB = RGB { _rgb :: (Int,Int,Int) } deriving (Eq,Show)

type YCbCrHist = Histogram CbCr

type RGBHist = Histogram RGB

{-

A histogram can have:
- Varying axes
- Varying bin sizes

A histogram must:
- Know how many pixels it accounts for
- Be indexable

A bin must:
- Know its own colour range? (for Inc.Int.?)

NOTES
-----

The shape of the image doesn't matter at all, so a `Vector` of pixels
is sufficient.

Remember that model and image resolutions also don't really matter,
but having a model with higher resolution than the target is better.

Dimensionality of Histogram only matters upon creation.

Leverage the type system so that no Histograms of different dimension
or colour space can be mixed!

-}  
