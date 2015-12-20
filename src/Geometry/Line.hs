module Geometry.Line where

import Geometry.Point


data Line a = L (Point a) (Point a)
  deriving (Eq, Ord)

instance (Show a) => Show (Line a) where
  show (L p1 p2) = "(L " ++ show p1 ++ " " ++ show p2 ++ ")"


-- Rotate line 90° clockwise about the origin
rotateLine90c :: (Num a) => Line a -> Line a
rotateLine90c (L p1 p2) =
    L (rotatePoint90c p1) (rotatePoint90c p2)

-- Rotate line 180° clockwise about the origin
rotateLine180c :: (Num a) => Line a -> Line a
rotateLine180c (L p1 p2) =
    L (rotatePoint180c p1) (rotatePoint180c p2)

-- Rotate line 270° clockwise about the origin
rotateLine270c :: (Num a) => Line a -> Line a
rotateLine270c (L p1 p2) =
    L (rotatePoint270c p1) (rotatePoint270c p2)

-- Reflect line about the line x = -y
reflectLineXMinusY :: (Num a) => Line a -> Line a
reflectLineXMinusY (L p1 p2) =
    L (reflectPointXMinusY p1) (reflectPointXMinusY p2)

-- Reflect line about the x axis
reflectLineXAxis :: (Num a) => Line a -> Line a
reflectLineXAxis (L p1 p2) =
    L (reflectPointXAxis p1) (reflectPointXAxis p2)


lineLength :: (Floating a) => Line a -> a
lineLength (L (P p1x p1y) (P p2x p2y)) = sqrt (dx * dx + dy * dy)
  where
    dx = p2x - p1x
    dy = p2y - p1y
