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
       ( Histogram(..)
       ) where

--import Data.HashMap.Strict
import Data.Key
import Data.Ratio
--import Data.Vector

---

-- | Any datatype which represents a Histogram, which is also a
-- Keyed and Zippable `Functor` and `Foldable`.
class (Foldable t, ZipWithKey t) => Histogram t where
  -- | The number of pixels in the histogram.
  pixelCount :: t Int -> Int
  pixelCount = sum

  -- | Intersection normalized by the number of pixels in the given model's
  -- `Histogram`. `Ratio` is used to prioritise accuracy.
  intersection :: t Int -> t Int -> Ratio Int
  intersection i m = inter % pixelCount m
    where inter = sum $ zipWithKey (\_ i' m' -> min i' m') i m
  
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

-}  


