module Main where

import Data.Colour.Histogram
import Test.Tasty
import Test.Tasty.HUnit
       
---

suite :: TestTree
suite = testGroup "Unit Tests"
  [ testGroup "binNxN Sanity Checks"
    [ testCase "(0,0) -> (0,0)" $ bin8x8 (0,0) @=? (0,0)
    , testCase "(255,255) -> (7,7)" $ bin8x8 (255,255) @=? (7,7)
    ]
  , testGroup "bin8x8f . rgbConstancy"
    [ testCase "(0,0,0)" $ (0,0) @=? bin8x8f (rgbConstancy (0,0,0))
    , testCase "(255,255,255)" $ (7,7) @=? bin8x8f (rgbConstancy (255,255,255))
    ]
  ]

main :: IO ()
main = defaultMain suite
