{-# LANGUAGE NamedFieldPuns #-}

-- | Geometric utility functions.
module Geometry
  ( Point(..)
  , Line(..)
  , Angle(..)
  , intersectLineLine
  , distanceFromOrigin
  , distanceLineFromOrigin
  , perpendicularThroughPoint
  , angleFromOrigin
  , normalizeAngle
  , lengthOfRayUntilIntersect
  ) where

import           Data.Fixed (mod')

data Point =
  Point Double
        Double

data Line = Line
  { slope     :: Double
  , intercept :: Double
  }

newtype Angle =
  Radians Double

intersectLineLine :: Line -> Line -> Point
intersectLineLine a b =
  let x = (intercept a - intercept b) / (slope b - slope a)
      y = slope a * x + intercept a
  in Point x y

distanceFromOrigin :: Point -> Double
distanceFromOrigin (Point x y) = sqrt (x * x + y * y)

distanceLineFromOrigin :: Line -> Double
distanceLineFromOrigin Line {slope, intercept} =
  abs intercept / sqrt (slope * slope + 1)

perpendicularThroughPoint :: Line -> Point -> Line
perpendicularThroughPoint Line {slope} (Point x y) =
  let slope' = -1 / slope
      intercept' = y - slope' * x
  in Line {slope = slope', intercept = intercept'}

angleFromOrigin :: Point -> Angle
angleFromOrigin (Point x y) = Radians $ atan2 y x

normalizeAngle :: Angle -> Angle
normalizeAngle (Radians theta) = Radians $ mod' theta (2 * pi)

lengthOfRayUntilIntersect :: Angle -> Line -> Double
lengthOfRayUntilIntersect (Radians theta) Line {slope, intercept} =
  intercept / (sin theta - slope * cos theta)
