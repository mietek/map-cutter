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
  { tRoadLinks          :: [RoadLink]
  , tRoadNodes          :: [RoadNode]
  , tLocalMinLength     :: Double
  , tLocalMaxLength     :: Double
  , tLocalMeanLength    :: Double
  , tLocalStdDevLength  :: Double
  , tGlobalMinLength    :: Double
  , tGlobalMaxLength    :: Double
  , tGlobalMeanLength   :: Double
  , tGlobalStdDevLength :: Double
  }

instance ToJSON Tile where
  toJSON t =
      J.object
        [ "roadLinks"          .= tRoadLinks t
        , "roadNodes"          .= tRoadNodes t
        , "localMinLength"     .= tLocalMinLength t
        , "localMaxLength"     .= tLocalMaxLength t
        , "localMeanLength"    .= tLocalMeanLength t
        , "localStdDevLength"  .= tLocalStdDevLength t
        , "globalMinLength"    .= tGlobalMinLength t
        , "globalMaxLength"    .= tGlobalMaxLength t
        , "globalMeanLength"   .= tGlobalMeanLength t
        , "globalStdDevLength" .= tGlobalStdDevLength t
        ]


newTile :: Tile
newTile =
    T { tRoadLinks          = []
      , tRoadNodes          = []
      , tLocalMinLength     = 0
      , tLocalMaxLength     = 0
      , tLocalMeanLength    = 0
      , tLocalStdDevLength  = 0
      , tGlobalMinLength    = 0
      , tGlobalMaxLength    = 0
      , tGlobalMeanLength   = 0
      , tGlobalStdDevLength = 0
      }

addRoadLink :: RoadLink -> Tile -> Tile
addRoadLink rl t =
    t { tRoadLinks = rl : (tRoadLinks t)
      }

addRoadNode :: RoadNode -> Tile -> Tile
addRoadNode rn t =
    t { tRoadNodes = rn : (tRoadNodes t)
      }

setLocalData :: Double -> Double -> Double -> Double -> Tile -> Tile
setLocalData lmin lmax lmean lstddev t =
    t { tLocalMinLength    = lmin
      , tLocalMaxLength    = lmax
      , tLocalMeanLength   = lmean
      , tLocalStdDevLength = lstddev
      }

setGlobalData :: Double -> Double -> Double -> Double -> Tile -> Tile
setGlobalData gmin gmax gmean gstddev t =
    t { tGlobalMinLength    = gmin
      , tGlobalMaxLength    = gmax
      , tGlobalMeanLength   = gmean
      , tGlobalStdDevLength = gstddev
      }


outputTile :: FilePath -> (Int, Int) -> Tile -> IO ()
outputTile out tc =
    L.writeFile (out </> tileFileName tc) . J.encode


tileName :: (Int, Int) -> String
tileName (tx, ty) =
    "tile-" ++ show tx ++ "-" ++ show ty

tileFileName :: (Int, Int) -> FilePath
tileFileName tc =
    tileName tc ++ ".json"

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
