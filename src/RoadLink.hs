{-# LANGUAGE OverloadedStrings #-}

module RoadLink where

import Data.Aeson ((.=), ToJSON, toJSON)
import qualified Data.Aeson as J
import Data.Text.Lazy (Text)

import Geometry.Polyline


data RoadLink = RL Text (Polyline Double)
  deriving (Eq, Show, Ord)

instance ToJSON RoadLink where
  toJSON (RL toid ps) = J.object ["toid" .= toid, "ps" .= ps]
