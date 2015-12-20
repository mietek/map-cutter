{-# LANGUAGE OverloadedStrings #-}

module RoadNode where

import Data.Aeson ((.=), ToJSON, toJSON)
import qualified Data.Aeson as J
import Data.Text.Lazy (Text)

import Geometry.Point


data RoadNode = RN
    { rnTOID  :: Text
    , rnPoint :: Point Double
    }
  deriving (Eq, Show, Ord)

instance ToJSON RoadNode where
  toJSON rn = J.object
    [ "toid" .= rnTOID rn
    , "p"    .= rnPoint rn
    ]
