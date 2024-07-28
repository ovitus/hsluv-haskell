module Util where

tripleDotProduct ::
     (Double, Double, Double) -> (Double, Double, Double) -> Double
tripleDotProduct (a, b, c) (d, e, f) = a * d + b * e + c * f

fromLinear :: Double -> Double
fromLinear c =
  if c <= 0.0031308
    then 12.92 * c
    else 1.055 * (c ** (1 / 2.4)) - 0.055

toLinear :: Double -> Double
toLinear c =
  if c >= 0.04045
    then ((c + 0.055) / (1 + 0.055)) ** 2.4
    else c / 12.92
