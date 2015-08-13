{-# LANGUAGE OverloadedStrings #-}

module Geometry.Point where

import Data.Aeson ((.=), ToJSON, toJSON)
import qualified Data.Aeson as J


data Point a = P a a
  deriving (Eq, Ord)

instance (Show a) => Show (Point a) where
  show (P x y) = "(P " ++ show x ++ " " ++ show y ++ ")"

instance (ToJSON a) => ToJSON (Point a) where
  toJSON (P x y) = J.object ["x" .= x, "y" .= y]


-- Rotate point 90° clockwise about the origin
rotatePoint90c :: (Num a) => Point a -> Point a
rotatePoint90c (P x y) =
    P y (-x)

-- Rotate point 180° clockwise about the origin
rotatePoint180c :: (Num a) => Point a -> Point a
rotatePoint180c (P x y) =
    P (-x) (-y)

-- Rotate point 270° clockwise about the origin
rotatePoint270c :: (Num a) => Point a -> Point a
rotatePoint270c (P x y) =
    P (-y) x

-- Reflect point about the line x = -y
reflectPointXMinusY :: (Num a) => Point a -> Point a
reflectPointXMinusY (P x y) =
    P (-y) (-x)

-- Reflect point about the x axis
reflectPointXAxis :: (Num a) => Point a -> Point a
reflectPointXAxis (P x y) =
    P x (-y)
