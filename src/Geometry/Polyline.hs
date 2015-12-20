module Geometry.Polyline where

import Data.Aeson (ToJSON, toJSON)

import Geometry.Line
import Geometry.Point


data Polyline a = PL [Point a]
  deriving (Eq, Ord)

instance (Show a) => Show (Polyline a) where
  show (PL ps) = "(PL " ++ show ps ++ ")"

instance (ToJSON a) => ToJSON (Polyline a) where
  toJSON (PL ps) = toJSON ps



polylineLength :: (Floating a) => Polyline a -> a
polylineLength (PL [])  = 0
polylineLength (PL [_]) = 0
polylineLength (PL (p1 : l@(p2 : _))) =
    lineLength (L p1 p2) + polylineLength (PL l)
