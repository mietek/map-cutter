module Geometry.Rect where

import Geometry.Point


data Rect a = R a a a a
  deriving (Eq, Ord)

instance (Show a) => Show (Rect a) where
  show (R left top right bottom) = "(R " ++ show (P left top) ++ " " ++ show (P right bottom) ++ ")"


-- Rotate rect 90° clockwise about the origin
rotateRect90c :: (Num a) => Rect a -> Rect a
rotateRect90c (R left top right bottom) =
    R bottom (-left) top (-right)

-- Rotate rect 180° clockwise about the origin
rotateRect180c :: (Num a) => Rect a -> Rect a
rotateRect180c (R left top right bottom) =
    R (-right) (-bottom) (-left) (-top)

-- Rotate rect 270° clockwise about the origin
rotateRect270c :: (Num a) => Rect a -> Rect a
rotateRect270c (R left top right bottom) =
    R (-top) right (-bottom) left

-- Reflect rect about the line x = -y
reflectRectXMinusY :: (Num a) => Rect a -> Rect a
reflectRectXMinusY (R left top right bottom) =
    R (-top) (-left) (-bottom) (-right)

-- Reflect rect about the x axis
reflectRectXAxis :: (Num a) => Rect a -> Rect a
reflectRectXAxis (R left top right bottom) =
    R left (-bottom) right (-top)

-- Reverse the direction of the y axis in rect
reverseRectYAxis :: (Num a) => Rect a -> Rect a
reverseRectYAxis (R left top right bottom) =
    R left bottom right top
