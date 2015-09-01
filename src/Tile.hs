{-# LANGUAGE OverloadedStrings #-}

module Tile where

import Data.Aeson ((.=), ToJSON, toJSON)
import qualified Data.Aeson as J
import qualified Data.ByteString.Lazy.Char8 as L
import System.FilePath ((</>))

import Geometry.Point
import Geometry.Rect
import RoadLink
import RoadNode


data Tile = T
  { tRoadLinks :: [RoadLink]
  , tRoadNodes :: [RoadNode]
  }

instance ToJSON Tile where
  toJSON (T ls ns) = J.object ["roadLinks" .= ls, "roadNodes" .= ns]


newTile :: Tile
newTile =
    T [] []

addPolyline :: RoadLink -> Tile -> Tile
addPolyline l (T ls ns) =
    T (l : ls) ns

addPoint :: RoadNode -> Tile -> Tile
addPoint n (T ls ns) =
    T ls (n : ns)


outputTile :: FilePath -> (Int, Int) -> Tile -> IO ()
outputTile out tc =
    L.writeFile (out </> tileFileName tc) . J.encode


tileFileName :: (Int, Int) -> FilePath
tileFileName (tx, ty) =
    "tile-" ++ show tx ++ "-" ++ show ty ++ ".json"

tileCoords :: (Int, Int) -> Point Double -> (Int, Int)
tileCoords (width, height) (P x y) =
    (tx, ty)
  where
    tx = floor (x / fromIntegral width)
    ty = floor (y / fromIntegral height)

tileBounds :: (Int, Int) -> (Int, Int) -> Rect Double
tileBounds (width, height) (tx, ty) =
    R left top right bottom
  where
    left   = fromIntegral (width  * tx)
    top    = fromIntegral (height * ty)
    right  = fromIntegral (width  * (tx + 1))
    bottom = fromIntegral (height * (ty + 1))
