module Main where

import Data.Colour.Histogram
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC
       
---

suite :: TestTree
suite = testGroup "Unit Tests"
  [ testGroup "binNxN Sanity Checks"
    [ testCase "(0,0) -> (0,0)" $ (0,0) @?= bin8x8 (0,0)
    , testCase "(255,255) -> (7,7)" $ (7,7) @?= bin8x8 (255,255)
    , testCase "Even distribution" evenDist0
    , QC.testProperty "No outliers" $ (< (8,8)) . bin8x8
    ]
  , testGroup "bin8x8f . rgbConstancy"
    [ testCase "(0,0,0) -> (0,0)" $ bin8x8f (rgbConstancy (0,0,0)) @?= (0,0)
    , testCase "(255,255,255) -> (7,7)" $ bin8x8f (rgbConstancy (255,255,255)) @?= (7,7)
    , testCase "Even distribution" evenDist1
    , QC.testProperty "No outliers" $ (< (8,8)) . bin8x8f . rgbConstancy
    ]
  ]

evenDist0 :: Assertion
evenDist0 = r @?= [(0,0),(1,1),(2,2),(3,3),(4,4),(5,5),(6,6),(7,7)]
  where r = map bin8x8 $ zip v v
        v = take 8 [31,63..]

evenDist1 :: Assertion
evenDist1 = r @?= [(0,0),(1,1),(2,2),(3,3),(4,4),(5,5),(6,6),(7,7)]
  where r = map bin8x8f $ zip v v
        v = [0.04,0.08,0.12,0.16,0.20,0.24,0.28,0.32]

main :: IO ()
main = defaultMain suite
