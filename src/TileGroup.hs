{-# LANGUAGE OverloadedStrings #-}

module TileGroup where

import qualified Data.Aeson as J
import qualified Data.ByteString.Lazy.Char8 as L
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M
import System.FilePath ((</>))

import Tile


type TileGroup = HashMap String Tile


newTileGroup :: TileGroup
newTileGroup =
    M.empty

addTile :: Tile -> (Int, Int) -> TileGroup -> TileGroup
addTile t tc =
    M.insert (tileName tc) t


outputTileGroup :: FilePath -> (Int, Int) -> TileGroup -> IO ()
outputTileGroup out tgc =
    L.writeFile (out </> tileGroupFileName tgc) . J.encode


tileGroupFileName :: (Int, Int) -> FilePath
tileGroupFileName (tgx, tgy) =
    "tile-group-" ++ show tgx ++ "-" ++ show tgy ++ ".json"

tileGroupCoords :: (Int, Int) -> (Int, Int)
tileGroupCoords (tx, ty) =
    (tgx, tgy)
  where
    tgx = tx `div` 10
    tgy = ty `div` 10
