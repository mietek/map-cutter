{-# LANGUAGE OverloadedStrings #-}

module RoadLink where

import Data.Aeson ((.=), ToJSON, toJSON)
import qualified Data.Aeson as J
import Data.Text.Lazy (Text)

import Geometry.Polyline


data RoadLink = RL
    { rlTOID    :: Text
    , rlPoints  :: Polyline Double
    , rlLength  :: Double
    , rlNegNode :: Text
    , rlPosNode :: Text
    }
  deriving (Eq, Show, Ord)

instance ToJSON RoadLink where
  toJSON rl = J.object
    [ "toid"   .= rlTOID rl
    , "ps"     .= rlPoints rl
    , "length" .= rlLength rl
    ]
