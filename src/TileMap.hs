module TileMap where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M

import Geometry.Point
import Geometry.Polyline
import Tile


type TileMap = HashMap (Int, Int) Tile


newTileMap :: TileMap
newTileMap =
    M.empty

insertPolylines :: TileMap -> [(Polyline Double, (Int, Int))] -> TileMap
insertPolylines =
    foldr (uncurry insertPolyline)

insertPoints :: TileMap -> [(Point Double, (Int, Int))] -> TileMap
insertPoints =
    foldr (uncurry insertPoint)

insertPolyline :: Polyline Double -> (Int, Int) -> TileMap -> TileMap
insertPolyline =
    updateTileMap . addPolyline

insertPoint :: Point Double -> (Int, Int) -> TileMap -> TileMap
insertPoint =
    updateTileMap . addPoint

updateTileMap :: (Tile -> Tile) -> (Int, Int) -> TileMap -> TileMap
updateTileMap fun tc tm =
    M.insert tc (fun (M.lookupDefault newTile tc tm)) tm


outputTileMap :: FilePath -> TileMap -> IO ()
outputTileMap out =
    mapM_ (uncurry (outputTile out)) . M.toList
