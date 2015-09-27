module TileGroupMap where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M

import Tile
import TileGroup


type TileGroupMap = HashMap (Int, Int) TileGroup


newTileGroupMap :: TileGroupMap
newTileGroupMap =
    M.empty

insertTiles :: TileGroupMap -> [((Int, Int), Tile)] -> TileGroupMap
insertTiles =
    foldr (uncurry (flip insertTile))

insertTile :: Tile -> (Int, Int) -> TileGroupMap -> TileGroupMap
insertTile t tc =
    updateTileGroupMap (addTile t tc) tc

updateTileGroupMap :: (TileGroup -> TileGroup) -> (Int, Int) -> TileGroupMap -> TileGroupMap
updateTileGroupMap fun tc tgm =
    M.insert tgc (fun tg) tgm
  where
    tg = M.lookupDefault newTileGroup tgc tgm
    tgc = tileGroupCoords tc


outputTileGroupMap :: FilePath -> TileGroupMap -> IO ()
outputTileGroupMap out =
    mapM_ (uncurry (outputTileGroup out)) . M.toList
