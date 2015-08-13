{-# LANGUAGE OverloadedStrings #-}

module Tile where

import Data.Aeson ((.=), ToJSON, toJSON)
import qualified Data.Aeson as J
import qualified Data.ByteString.Lazy.Char8 as L
import System.FilePath ((</>))

import Geometry.Point
import Geometry.Polyline
import Geometry.Rect


data Tile = T
  { tPolylines :: [Polyline Double]
  , tPoints    :: [Point Double]
  }

instance ToJSON Tile where
  toJSON (T ls ps) = J.object ["polylines" .= ls, "points" .= ps]


newTile :: Tile
newTile =
    T [] []

addPolyline :: Polyline Double -> Tile -> Tile
addPolyline l (T ls ps) =
    T (l : ls) ps

addPoint :: Point Double -> Tile -> Tile
addPoint p (T ls ps) =
    T ls (p : ps)


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
