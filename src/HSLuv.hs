-- | Haskell port of the perceptually-uniform HSLuv colorspace model (http://www.hsluv.org/).
module HSLuv where

import           Constants           (epsilon, kappa, m, minv, refU, refV, refY)
import           Control.Applicative (liftA2)
import           Data.Colour.CIE     (Colour, cieXYZ, cieXYZView)
import           Data.Fixed          (mod')
import qualified Geometry            as G
import           Numeric             (readHex, showHex)
import           Util                (fromLinear, toLinear, tripleDotProduct)

-- | Value in [0, 1].
newtype RGBRed =
  RGBRed Double
  deriving (Eq, Show)

-- | Value in [0, 1].
newtype RGBGreen =
  RGBGreen Double
  deriving (Eq, Show)

-- | Value in [0, 1].
newtype RGBBlue =
  RGBBlue Double
  deriving (Eq, Show)

-- | Values in [0, 1].
data RGB =
  RGB RGBRed
      RGBGreen
      RGBBlue
  deriving (Eq, Show)

-- | Value in [0, 1].
newtype XYZX =
  XYZX Double
  deriving (Eq, Show)

-- | Value in [0, 1].
newtype XYZY =
  XYZY Double
  deriving (Eq, Show)

-- | Value in [0, 1].
newtype XYZZ =
  XYZZ Double
  deriving (Eq, Show)

-- | Values in [0, 1].
data XYZ =
  XYZ XYZX
      XYZY
      XYZZ
  deriving (Eq, Show)

newtype LUVLightness =
  LUVLightness Double
  deriving (Eq, Show)

newtype LUVU =
  LUVU Double
  deriving (Eq, Show)

newtype LUVV =
  LUVV Double
  deriving (Eq, Show)

data LUV =
  LUV LUVLightness
      LUVU
      LUVV
  deriving (Eq, Show)

newtype LCHLightness =
  LCHLightness Double
  deriving (Eq, Show)

newtype LCHChroma =
  LCHChroma Double
  deriving (Eq, Show)

newtype LCHHue =
  LCHHue Double
  deriving (Eq, Show)

data LCH =
  LCH LCHLightness
      LCHChroma
      LCHHue
  deriving (Eq, Show)

-- | Value in [0, 360].
newtype HSLuvHue =
  HSLuvHue Double
  deriving (Eq, Show)

-- | Value in [0, 100].
newtype HSLuvSaturation =
  HSLuvSaturation Double
  deriving (Eq, Show)

-- | Value in [0, 100].
newtype HSLuvLightness =
  HSLuvLightness Double
  deriving (Eq, Show)

-- | Values in [0, 360]; [0, 100]; [0, 100].
data HSLuv =
  HSLuv HSLuvHue
        HSLuvSaturation
        HSLuvLightness
  deriving (Eq, Show)

-- | Value in [0, 360].
newtype HPLuvHue =
  HPLuvHue Double
  deriving (Eq, Show)

-- | Value in [0, 100].
newtype HPLuvPastel =
  HPLuvPastel Double
  deriving (Eq, Show)

-- | Value in [0, 100].
newtype HPLuvLightness =
  HPLuvLightness Double
  deriving (Eq, Show)

-- | Values in [0, 360]; [0, 100]; [0, 100].
data HPLuv =
  HPLuv HPLuvHue
        HPLuvPastel
        HPLuvLightness
  deriving (Eq, Show)

-- | For a given lightness, return a list of 6 lines in slope-intercept
-- form that represent the bounds in CIELUV, stepping over which will
-- push a value out of the RGB gamut.
getBounds :: HSLuvLightness -> [G.Line]
getBounds (HSLuvLightness l) =
  let sub1 = ((l + 16) ** 3) / 1560896
      sub2 =
        if sub1 > epsilon
          then sub1
          else l / kappa
  in liftA2 (bounds sub2) m [0, 1]
  where
    bounds sub2 (m1, m2, m3) t =
      let top1 = (284517 * m1 - 94839 * m3) * sub2
          top2 =
            (838422 * m3 + 769860 * m2 + 731718 * m1) * l * sub2 -
            769860 * t * l
          bottom = (632260 * m3 - 126452 * m2) * sub2 + 126452 * t
      in G.Line {G.slope = top1 / bottom, G.intercept = top2 / bottom}

-- | For given lightness, returns the maximum chroma. Keeping the chroma value
-- below this number will ensure that for any hue, the color is within the RGB
-- gamut.
maxSafeChromaForL :: HSLuvLightness -> LCHChroma
maxSafeChromaForL l' =
  LCHChroma $ minimum $ map G.distanceLineFromOrigin $ getBounds l'

maxChromaForLH :: HSLuvLightness -> HSLuvHue -> LCHChroma
maxChromaForLH l' (HSLuvHue h) =
  let hrad = G.Radians $ h / 360 * pi * 2
  in LCHChroma .
     minimum .
     (:) (1 / 0) . filter (>= 0) . map (G.lengthOfRayUntilIntersect hrad) $
     getBounds l'

xyzToRgb :: XYZ -> RGB
xyzToRgb (XYZ (XYZX x) (XYZY y) (XYZZ z)) =
  let [r, g, b] = map (fromLinear . tripleDotProduct (x, y, z)) m
  in RGB (RGBRed r) (RGBGreen g) (RGBBlue b)

rgbToXyz :: RGB -> XYZ
rgbToXyz (RGB (RGBRed r) (RGBGreen g) (RGBBlue b)) =
  let [x, y, z] =
        map (tripleDotProduct (toLinear r, toLinear g, toLinear b)) minv
  in XYZ (XYZX x) (XYZY y) (XYZZ z)

yToL :: XYZY -> LUVLightness
yToL (XYZY y) =
  if y <= epsilon
    then LUVLightness $ (y / refY) * kappa
    else LUVLightness $ 116 * ((y / refY) ** (1 / 3)) - 16

lToY :: LUVLightness -> XYZY
lToY (LUVLightness l) =
  if l <= 8
    then XYZY $ refY * l / kappa
    else XYZY $ refY * (((l + 16) / 116) ** 3)

xyzToLuv :: XYZ -> LUV
xyzToLuv (XYZ (XYZX x) y'@(XYZY y) (XYZZ z)) =
  let divider = (x + (15 * y) + (3 * z))
      varU = (4 * x) / divider
      varV = (9 * y) / divider
      l'@(LUVLightness l) = yToL y'
      u = 13 * l * (varU - refU)
      v = 13 * l * (varV - refV)
  in if l == 0
       then LUV (LUVLightness 0) (LUVU 0) (LUVV 0)
       else LUV l' (LUVU u) (LUVV v)

luvToXyz :: LUV -> XYZ
luvToXyz (LUV (LUVLightness 0) _ _) = XYZ (XYZX 0) (XYZY 0) (XYZZ 0)
luvToXyz (LUV l'@(LUVLightness l) (LUVU u) (LUVV v)) =
  let varU = u / (13 * l) + refU
      varV = v / (13 * l) + refV
      (XYZY y) = lToY l'
      x = -(9 * y * varU) / ((varU - 4) * varV - (varU * varV))
      z = (9 * y - (15 * varV * y) - (varV * x)) / (3 * varV)
  in XYZ (XYZX x) (XYZY y) (XYZZ z)

luvToLch :: LUV -> LCH
luvToLch (LUV (LUVLightness l) (LUVU u) (LUVV v)) =
  let c = sqrt $ u * u + v * v
      h =
        if c < 0.00000001
          then 0
          else (atan2 v u * 180 / pi) `mod'` 360
  in LCH (LCHLightness l) (LCHChroma c) (LCHHue h)

lchToLuv :: LCH -> LUV
lchToLuv (LCH (LCHLightness l) (LCHChroma c) (LCHHue h)) =
  let hrad = h / 360 * 2 * pi
  in LUV (LUVLightness l) (LUVU $ c * cos hrad) (LUVV $ c * sin hrad)

hsluvToLchWith :: (HSLuvLightness -> HSLuvHue -> LCHChroma) -> HSLuv -> LCH
hsluvToLchWith f (HSLuv h'@(HSLuvHue h) (HSLuvSaturation s) l'@(HSLuvLightness l))
  | l > 99.9999999 = LCH (LCHLightness 100) (LCHChroma 0) (LCHHue h)
  | l < 0.00000001 = LCH (LCHLightness 0) (LCHChroma 0) (LCHHue h)
  | otherwise =
    let (LCHChroma c) = f l' h'
    in LCH (LCHLightness l) (LCHChroma (c / 100 * s)) (LCHHue h)

lchToHsluvWith :: (HSLuvLightness -> HSLuvHue -> LCHChroma) -> LCH -> HSLuv
lchToHsluvWith f (LCH (LCHLightness l) (LCHChroma c) (LCHHue h))
  | l > 99.9999999 = HSLuv (HSLuvHue h) (HSLuvSaturation 0) (HSLuvLightness 100)
  | l < 0.00000001 = HSLuv (HSLuvHue h) (HSLuvSaturation 0) (HSLuvLightness 0)
  | otherwise =
    let (LCHChroma maxC) = f (HSLuvLightness l) (HSLuvHue h)
    in HSLuv (HSLuvHue h) (HSLuvSaturation (c / maxC * 100)) (HSLuvLightness l)

hsluvToLch :: HSLuv -> LCH
hsluvToLch = hsluvToLchWith maxChromaForLH

lchToHsluv :: LCH -> HSLuv
lchToHsluv = lchToHsluvWith maxChromaForLH

hpluvToLch :: HPLuv -> LCH
hpluvToLch (HPLuv (HPLuvHue h) (HPLuvPastel p) (HPLuvLightness l)) =
  hsluvToLchWith
    (\l' _ -> maxSafeChromaForL l')
    (HSLuv (HSLuvHue h) (HSLuvSaturation p) (HSLuvLightness l))

lchToHpluv :: LCH -> HPLuv
lchToHpluv lch =
  let (HSLuv (HSLuvHue h) (HSLuvSaturation s) (HSLuvLightness l)) =
        lchToHsluvWith (\l' _ -> maxSafeChromaForL l') lch
  in HPLuv (HPLuvHue h) (HPLuvPastel s) (HPLuvLightness l)

rgbToHex :: RGB -> String
rgbToHex (RGB (RGBRed r) (RGBGreen g) (RGBBlue b)) =
  "#" ++ toHex r ++ toHex g ++ toHex b
  where
    leftPad s n c = replicate (max 0 (n - length s)) c ++ s
    toHex c = leftPad (showHex (round $ c * 255 :: Integer) "") 2 '0'

hexToRgb :: String -> Maybe RGB
hexToRgb ['#', a, b, c, d, e, f] =
  case (readHex [a, b], readHex [c, d], readHex [e, f]) of
    ([(rr, rr')], [(rg, rg')], [(rb, rb')]) ->
      if any (/= "") [rr', rg', rb']
        then Nothing
        else Just $
             RGB (RGBRed (rr / 255)) (RGBGreen (rg / 255)) (RGBBlue (rb / 255))
    _ -> Nothing
hexToRgb _ = Nothing

lchToRgb :: LCH -> RGB
lchToRgb = xyzToRgb . luvToXyz . lchToLuv

rgbToLch :: RGB -> LCH
rgbToLch = luvToLch . xyzToLuv . rgbToXyz

hsluvToRgb :: HSLuv -> RGB
hsluvToRgb = lchToRgb . hsluvToLch

rgbToHsluv :: RGB -> HSLuv
rgbToHsluv = lchToHsluv . rgbToLch

hpluvToRgb :: HPLuv -> RGB
hpluvToRgb = lchToRgb . hpluvToLch

rgbToHpluv :: RGB -> HPLuv
rgbToHpluv = lchToHpluv . rgbToLch

hsluvToHex :: HSLuv -> String
hsluvToHex = rgbToHex . hsluvToRgb

hpluvToHex :: HPLuv -> String
hpluvToHex = rgbToHex . hpluvToRgb

hexToHsluv :: String -> Maybe HSLuv
hexToHsluv = fmap rgbToHsluv . hexToRgb

hexToHpluv :: String -> Maybe HPLuv
hexToHpluv = fmap rgbToHpluv . hexToRgb

hsluvToColour :: HSLuv -> Colour Double
hsluvToColour hsluv =
  let XYZ (XYZX x) (XYZY y) (XYZZ z) = luvToXyz . lchToLuv . hsluvToLch $ hsluv
  in cieXYZ x y z

colourToHsluv :: Colour Double -> HSLuv
colourToHsluv colour =
  let (x, y, z) = cieXYZView colour
  in lchToHsluv . luvToLch . xyzToLuv $ XYZ (XYZX x) (XYZY y) (XYZZ z)

hpluvToColour :: HPLuv -> Colour Double
hpluvToColour hsluv =
  let XYZ (XYZX x) (XYZY y) (XYZZ z) = luvToXyz . lchToLuv . hpluvToLch $ hsluv
  in cieXYZ x y z

colourToHpluv :: Colour Double -> HPLuv
colourToHpluv colour =
  let (x, y, z) = cieXYZView colour
  in lchToHpluv . luvToLch . xyzToLuv $ XYZ (XYZX x) (XYZY y) (XYZZ z)
