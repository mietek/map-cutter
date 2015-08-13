module Geometry.LineClipping.Common where

import Geometry.Line
import Geometry.Point
import Geometry.Rect


delta :: (Num a) => Line a -> Point a
delta (L (P p1x p1y) (P p2x p2y)) =
    P (p2x - p1x) (p2y - p1y)


leftProduct :: (Num a) => Rect a -> Line a -> Point a -> a
leftProduct (R left _ _ _) (L (P p1x _) _) (P _ dy) =
    (left - p1x) * dy

topProduct :: (Num a) => Rect a -> Line a -> Point a -> a
topProduct (R _ top _ _) (L (P _ p1y) _) (P dx _) =
    (top - p1y) * dx

rightProduct :: (Num a) => Rect a -> Line a -> Point a -> a
rightProduct (R _ _ right _) (L (P p1x _) _) (P _ dy) =
    (right - p1x) * dy

bottomProduct :: (Num a) => Rect a -> Line a -> Point a -> a
bottomProduct (R _ _ _ bottom) (L (P _ p1y) _) (P dx _) =
    (bottom - p1y) * dx


clipLeft :: (Fractional a, Num a) => Rect a -> Line a -> Point a -> a -> Point a
clipLeft (R left _ _ _) (L (P _ p1y) _) (P dx _) leftP =
    P left (p1y + leftP / dx)

clipTop :: (Fractional a, Num a) => Rect a -> Line a -> Point a -> a -> Point a
clipTop (R _ top _ _) (L (P p1x _) _) (P _ dy) topP =
    P (p1x + topP / dy) top

clipRight :: (Fractional a, Num a) => Rect a -> Line a -> Point a -> a -> Point a
clipRight (R _ _ right _) (L (P _ p1y) _) (P dx _) rightP =
    P right (p1y + rightP / dx)

clipBottom :: (Fractional a, Num a) => Rect a -> Line a -> Point a -> a -> Point a
clipBottom (R _ _ _ bottom) (L (P p1x _) _) (P _ dy) bottomP =
    P (p1x + bottomP / dy) bottom
