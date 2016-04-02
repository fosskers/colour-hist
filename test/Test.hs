module Main where

import           Codec.Picture.Jpg
import           Codec.Picture.Types
import qualified Data.ByteString as B
import           Data.Colour.Histogram
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector.Storable as V
import           Data.Word (Word8)
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck as QC

---

type H = YCbCrHist
type H' = RGBHist

suite :: H -> H -> H -> H -> H -> H -> H -> H' -> H' -> H' -> TestTree
suite cat catT catO catC catB cali calB cat' catO' catC' = testGroup "Unit Tests"
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
      [ testCase "Same Model and Image: > 0.99" $ two (> 0.99) cat cat
      , testCase "Rotation: > 0.95" $ two (> 0.95) catT cat
      , testCase "Occlusion: > 0.75" $ two (> 0.75) catO cat
      , testCase "Model is twice as small: > 0.99" $ two (> 0.99) catB cat
      , testCase "Model is twice as large: ~0.25" $ two (> 0.249) cat catB
      ]
    , testGroup "Image vs matching and unmatching models"
      [ testCase "Same image" $ three cat cat cali
      , testCase "Rotation" $ three catT cat cali
      , testCase "Occlusion" $ three catO cat cali
      , testCase "Cut" $ three catC cat cali
      , testCase "Model is smaller" $ three catB cat cali
      , testCase "Model is larger" $ three cat catB calB
      ]
    , testGroup "YCbCr is better than RGB"
      [ testCase "Occlusion" $ ycbcrVsRgb catO cat catO' cat'
      , testCase "Cut" $ ycbcrVsRgb catC cat catC' cat'
      ]
    ]
  ]

two :: (Float -> Bool) -> H -> H -> Assertion
two n i m = assertBool ("Intersection was: " ++ show intr) $ n intr
  where intr = intersection i m

three :: H -> H -> H -> Assertion
three i m0 m1 = assertBool msg $ intr0 > intr1
  where intr0 = intersection i m0
        intr1 = intersection i m1
        msg = "Real: " ++ show intr0 ++ " | False: " ++ show intr1

ycbcrVsRgb :: H -> H -> H' -> H' -> Assertion
ycbcrVsRgb i0 m0 i1 m1 = assertBool msg $ intr0 > intr1
  where msg = ("YCbCr: " ++ show intr0 ++ " | RGB: " ++ show intr1)
        intr0 = intersection i0 m0
        intr1 = intersection i1 m1

readImg :: (Either String DynamicImage -> b) -> FilePath -> IO b
readImg f fp = f . decodeJpeg <$> B.readFile fp

ycbcr :: Either a DynamicImage -> V.Vector Word8
ycbcr (Right (ImageYCbCr8 i)) = imageData i
ycbcr _ = V.empty

rgb :: Either a DynamicImage -> V.Vector Word8
rgb (Right (ImageYCbCr8 i)) = imageData $ (convertImage i :: Image PixelRGB8)
rgb _ = V.empty

evenDist0 :: Assertion
evenDist0 = r @?= [(0,0),(1,1),(2,2),(3,3),(4,4),(5,5),(6,6),(7,7)]
  where r = map bin8x8 $ zip v v
        v = take 8 [31,63..]

evenDist1 :: Assertion
evenDist1 = r @?= [(0,0),(1,1),(2,2),(3,3),(4,4),(5,5),(6,6),(7,7)]
  where r = map bin8x8f $ zip v v
        v = [0.04,0.08,0.12,0.16,0.20,0.24,0.28,0.32]

main :: IO ()
main = do
  cat  <- ycbcrHist bin32x32 <$> readImg ycbcr "test/cat.jpg"
  catT <- ycbcrHist bin32x32 <$> readImg ycbcr "test/cat-turned.jpg"
  catO <- ycbcrHist bin32x32 <$> readImg ycbcr "test/cat-neko.jpg"
  catC <- ycbcrHist bin32x32 <$> readImg ycbcr "test/cat-cut.jpg"
  catB <- ycbcrHist bin32x32 <$> readImg ycbcr "test/cat-big.jpg"
  cali <- ycbcrHist bin32x32 <$> readImg ycbcr "test/calico.jpg"
  calB <- ycbcrHist bin32x32 <$> readImg ycbcr "test/calico-big.jpg"
  cat' <- rgbHist (bin32x32f . rgbConstancy) <$> readImg rgb "test/cat.jpg"
  catO' <- rgbHist (bin32x32f . rgbConstancy) <$> readImg rgb "test/cat-neko.jpg"
  catC' <- rgbHist (bin32x32f . rgbConstancy) <$> readImg rgb "test/cat-cut.jpg"
  putStrLn $ "Bins used in cat.jpg: " ++ show (HM.size $ _hm cat)
  defaultMain (suite cat catT catO catC catB cali calB cat' catO' catC')
