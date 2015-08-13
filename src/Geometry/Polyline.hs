module Geometry.Polyline where

import Data.Aeson (ToJSON, toJSON)

import Geometry.Point


data Polyline a = PL [Point a]
  deriving (Eq, Ord)

instance (Show a) => Show (Polyline a) where
  show (PL ps) = "(PL " ++ show ps ++ ")"

instance (ToJSON a) => ToJSON (Polyline a) where
  toJSON (PL ps) = toJSON ps
