import           Control.Exception.Base (assert)
import           Control.Monad          (liftM3)
import           Data.Aeson
import           Data.ByteString.Lazy   (readFile)
import           Data.Map               (Map, lookup, toList)
import           Data.Maybe             (fromJust)
import           HSLuv

maxDiff :: Double
maxDiff = 0.0000000001

maxRelDiff :: Double
maxRelDiff = 0.000000001

approxEqual :: Double -> Double -> Bool
approxEqual a b =
  let diff = abs (b - a)
  in diff <= maxDiff || diff <= maxRelDiff * max (abs a) (abs b)

approxEqualTriple ::
     (Double, Double, Double) -> (Double, Double, Double) -> Bool
approxEqualTriple (a, b, c) (d, e, f) =
  approxEqual a d && approxEqual b e && approxEqual c f

checkTestCase :: String -> TestCase -> Bool
checkTestCase hex cases =
  let rgb = fromJust $ Data.Map.lookup "rgb" cases
      xyz = fromJust $ Data.Map.lookup "xyz" cases
      luv = fromJust $ Data.Map.lookup "luv" cases
      lch = fromJust $ Data.Map.lookup "lch" cases
      hsluv = fromJust $ Data.Map.lookup "hsluv" cases
      hpluv = fromJust $ Data.Map.lookup "hpluv" cases
      rgb'@(RGB (RGBRed rgbr') (RGBGreen rgbg') (RGBBlue rgbb')) =
        fromJust $ hexToRgb hex
      xyz'@(XYZ (XYZX xyzx') (XYZY xyzy') (XYZZ xyzz')) = rgbToXyz rgb'
      luv'@(LUV (LUVLightness luvl') (LUVU luvu') (LUVV luvv')) = xyzToLuv xyz'
      lch'@(LCH (LCHLightness lchl') (LCHChroma lchc') (LCHHue lchh')) =
        luvToLch luv'
      hsluvLch'@(HSLuv (HSLuvHue hsluvLchh') (HSLuvSaturation hsluvLchs') (HSLuvLightness hsluvLchl')) =
        lchToHsluv lch'
      hpluvLch'@(HPLuv (HPLuvHue hpluvLchh') (HPLuvPastel hpluvLchp') (HPLuvLightness hpluvLchl')) =
        lchToHpluv lch'
      hsluvHex'@(HSLuv (HSLuvHue hsluvHexh') (HSLuvSaturation hsluvHexs') (HSLuvLightness hsluvHexl')) =
        fromJust $ hexToHsluv hex
      hpluvHex'@(HPLuv (HPLuvHue hpluvHexh') (HPLuvPastel hpluvHexp') (HPLuvLightness hpluvHexl')) =
        fromJust $ hexToHpluv hex
      lchHsluv'@(LCH (LCHLightness lchHsluvl') (LCHChroma lchHsluvc') (LCHHue lchHsluvh')) =
        hsluvToLch hsluvHex'
      lchHpluv'@(LCH (LCHLightness lchHpluvl') (LCHChroma lchHpluvc') (LCHHue lchHpluvh')) =
        hpluvToLch hpluvHex'
      luvLch'@(LUV (LUVLightness luvLchl') (LUVU luvLchu') (LUVV luvLchv')) =
        lchToLuv lchHsluv'
      xyzLuv'@(XYZ (XYZX xyzLuvx') (XYZY xyzLuvy') (XYZZ xyzLuvz')) =
        luvToXyz luvLch'
      rgbXyz'@(RGB (RGBRed rgbXyzr') (RGBGreen rgbXyzg') (RGBBlue rgbXyzb')) =
        xyzToRgb xyzLuv'
  in approxEqualTriple rgb (rgbr', rgbg', rgbb') &&
     approxEqualTriple xyz (xyzx', xyzy', xyzz') &&
     approxEqualTriple luv (luvl', luvu', luvv') &&
     approxEqualTriple lch (lchl', lchc', lchh') &&
     approxEqualTriple hsluv (hsluvLchh', hsluvLchs', hsluvLchl') &&
     approxEqualTriple hsluv (hsluvHexh', hsluvHexs', hsluvHexl') &&
     approxEqualTriple hpluv (hpluvLchh', hpluvLchp', hpluvLchl') &&
     approxEqualTriple hpluv (hpluvHexh', hpluvHexp', hpluvHexl') &&
     approxEqualTriple lch (lchHsluvl', lchHsluvc', lchHsluvh') &&
     approxEqualTriple lch (lchHpluvl', lchHpluvc', lchHpluvh') &&
     approxEqualTriple luv (luvLchl', luvLchu', luvLchv') &&
     approxEqualTriple xyz (xyzLuvx', xyzLuvy', xyzLuvz') &&
     approxEqualTriple rgb (rgbXyzr', rgbXyzg', rgbXyzb') &&
     hex == rgbToHex rgb' &&
     hex == hsluvToHex hsluvLch' &&
     hex == hpluvToHex hpluvLch' &&
     hex == hsluvToHex hsluvHex' && hex == hpluvToHex hpluvHex'

checkRgbBounds :: Bool
checkRgbBounds = all checkRgbBound $ liftM3 (,,) [0, 1] [0, 1] [0, 1]
  where
    checkRgbBound (r, g, b) =
      let RGB (RGBRed r') (RGBGreen g') (RGBBlue b') =
            hsluvToRgb . rgbToHsluv $ RGB (RGBRed r) (RGBGreen g) (RGBBlue b)
      in approxEqualTriple (r, g, b) (r', g', b')

type TestCase = Map String (Double, Double, Double)

type TestVectors = Map String TestCase

main :: IO ()
main = do
  testVectorsString <- Data.ByteString.Lazy.readFile "test/snapshot-rev4.json"
  let testVectors = fromJust (decode testVectorsString :: Maybe TestVectors)
  let vectorsCheck = all (uncurry checkTestCase) (Data.Map.toList testVectors)
  print $ assert (vectorsCheck && checkRgbBounds) "All tests pass."
