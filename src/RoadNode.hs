{-# LANGUAGE OverloadedStrings #-}

module RoadNode where

import Data.Aeson ((.=), ToJSON, toJSON)
import qualified Data.Aeson as J
import Data.Text.Lazy (Text)

import Geometry.Point


data RoadNode = RN Text (Point Double)
  deriving (Eq, Show, Ord)

instance ToJSON RoadNode where
  toJSON (RN toid p) = J.object ["toid" .= toid, "p" .= p]
