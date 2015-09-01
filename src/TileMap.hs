module TileMap where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M

import RoadLink
import RoadNode
import Tile


type TileMap = HashMap (Int, Int) Tile


newTileMap :: TileMap
newTileMap =
    M.empty

insertRoadLinks :: TileMap -> [(RoadLink, (Int, Int))] -> TileMap
insertRoadLinks =
    foldr (uncurry insertRoadLink)

insertRoadNodes :: TileMap -> [(RoadNode, (Int, Int))] -> TileMap
insertRoadNodes =
    foldr (uncurry insertRoadNode)

insertRoadLink :: RoadLink -> (Int, Int) -> TileMap -> TileMap
insertRoadLink =
    updateTileMap . addRoadLink

insertRoadNode :: RoadNode -> (Int, Int) -> TileMap -> TileMap
insertRoadNode =
    updateTileMap . addRoadNode

updateTileMap :: (Tile -> Tile) -> (Int, Int) -> TileMap -> TileMap
updateTileMap fun tc tm =
    M.insert tc (fun (M.lookupDefault newTile tc tm)) tm


outputTileMap :: FilePath -> TileMap -> IO ()
outputTileMap out =
    mapM_ (uncurry (outputTile out)) . M.toList
