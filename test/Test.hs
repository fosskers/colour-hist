module Main where

import           Codec.Picture.Jpg
import           Codec.Picture.Types
import qualified Data.ByteString as B
import           Data.Colour.Histogram
import qualified Data.Vector.Storable as V
import           Data.Word (Word8)
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck as QC

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
  , testGroup "intersection"
    [ testGroup "Image matched to tranformed self"
      [ testCase "Same Model and Image: > 0.99" selfId
      , testCase "Rotation: > 0.99" turned
      , testCase "Occlusion: > 0.75" occlusion
      , testCase "Model is twice as small: > 0.95" smaller
      , testCase "Model is twice as large: > 0.9" bigger
      ]
    , testGroup "Image vs matching and unmatching models"
      [ testCase "Same image" same
      , testCase "Occlusion" occlusion3
      , testCase "Model is smaller" smaller3
      , testCase "Model is larger" bigger3
      ]
    ]
  ]

two :: (Float -> Bool) -> FilePath -> FilePath -> Assertion
two n f1 f2 = do
  m <- ycbcrHist bin8x8 <$> readImg f1
  i <- ycbcrHist bin8x8 <$> readImg f2
  let intr = intersection i m
  assertBool ("Intersection was: " ++ show intr) $ n intr

three :: FilePath -> FilePath -> FilePath -> Assertion
three f0 f1 f2 = do
  m0 <- ycbcrHist bin32x32 <$> readImg f0
  m1 <- ycbcrHist bin32x32 <$> readImg f1
  i <- ycbcrHist bin32x32 <$> readImg f2
  let intr0 = intersection i m0
      intr1 = intersection i m1
  assertBool ("Real: " ++ show intr0 ++ " | False: " ++ show intr1) $ intr0 > intr1

-- | An image should have a perfect match to itself.
selfId :: Assertion
selfId = two (> 0.99) "test/cat.jpg" "test/cat.jpg"

-- | An image should have a high match to a rotated version of itself.
turned :: Assertion
turned = two (> 0.99) "test/cat.jpg" "test/cat-turned.jpg"

-- | An image should have a decent match to an occluded version of itself.
occlusion :: Assertion
occlusion = two (> 0.75) "test/cat.jpg" "test/cat-neko.jpg"

occlusion3 :: Assertion
occlusion3 = three "test/cat.jpg" "test/calico.jpg" "test/cat-neko.jpg"

smaller :: Assertion
smaller = two (> 0.95) "test/cat.jpg" "test/cat-big.jpg"

smaller3 :: Assertion
smaller3 = three "test/cat.jpg" "test/calico.jpg" "test/cat-big.jpg"

bigger :: Assertion
bigger = two (> 0.9) "test/cat-big.jpg" "test/cat.jpg"

bigger3 :: Assertion
bigger3 = three "test/cat-big.jpg" "test/calico-big.jpg" "test/cat.jpg"

same :: Assertion
same = three "test/cat.jpg" "test/calico.jpg" "test/cat.jpg"

readImg :: FilePath -> IO (V.Vector Word8)
readImg fp = ycbcr . decodeJpeg <$> B.readFile fp

ycbcr :: Either a DynamicImage -> V.Vector Word8
ycbcr (Right (ImageYCbCr8 i)) = imageData i
ycbcr _ = V.empty

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
