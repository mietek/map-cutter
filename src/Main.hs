module Main where

import Control.Applicative ((<$>))
import qualified Data.ByteString.Lazy.Char8 as L
import Data.ByteString.Lex.Fractional (readDecimal)
import Data.Maybe (catMaybes)
import Data.Text.Lazy.Encoding (decodeUtf8)
import System.Directory (createDirectoryIfMissing)

import Geometry.Line
import Geometry.LineClipping.Fast
import Geometry.Point
import Geometry.Polyline
import Options
import RoadLink
import RoadNode
import Tile
import TileMap


main :: IO ()
main = do
    opts <- getOptions
    ls <- L.lines <$> L.readFile (oRoadLinks opts)
    ns <- L.lines <$> L.readFile (oRoadNodes opts)
    let out  = oOutput opts
        size = (oWidth opts, oHeight opts)
        tm   = processRoadNodes (processRoadLinks newTileMap size ls) size ns
    createDirectoryIfMissing True out
    outputTileMap out tm


processRoadLinks :: TileMap -> (Int, Int) -> [L.ByteString] -> TileMap
processRoadLinks tm size =
    insertRoadLinks tm . concatMap (processRoadLink size) . catMaybes . map readRoadLink

processRoadNodes :: TileMap -> (Int, Int) -> [L.ByteString] -> TileMap
processRoadNodes tm size =
    insertRoadNodes tm . map (processRoadNode size) . catMaybes . map readRoadNode


processRoadLink :: (Int, Int) -> RoadLink -> [(RoadLink, (Int, Int))]
processRoadLink _ (RL _ (PL []))  = []
processRoadLink _ (RL _ (PL [_])) = []
processRoadLink size (RL toid (PL (p : ps@(_ : _)))) =
    let tc = tileCoords size p
        tb = tileBounds size tc
    in  loop tc tb p ps []
  where
    loop tc _ p1 [] qs = [((RL toid (PL (reverse (p1 : qs)))), tc)]
    loop tc@(tx, ty) tb p1 ps1@(p2 : ps2) qs =
        case reverseFastClipYAxis tb (L p1 p2) of
          (q2, P 0 0)   -> loop tc tb q2 ps2 (p1 : qs)
          (q2, P vx vy) -> let uc = (tx + vx, ty + vy)
                               ub = tileBounds size uc
                               rs = if q2 == p1
                                      then q2 : qs
                                      else q2 : p1 : qs
                           in  ((RL toid (PL (reverse rs))), tc) : loop uc ub q2 ps1 []

processRoadNode :: (Int, Int) -> RoadNode -> (RoadNode, (Int, Int))
processRoadNode size (RN toid p) =
    let tc = tileCoords size p
    in  ((RN toid p), tc)


readRoadLink :: L.ByteString -> Maybe RoadLink
readRoadLink s =
    case L.split ' ' s of
      (toid : ss) -> Just (RL (decodeUtf8 toid) (readPolyline ss))
      _ -> Nothing

readPolyline :: [L.ByteString] -> Polyline Double
readPolyline = PL . catMaybes . map readPoint

readRoadNode :: L.ByteString -> Maybe RoadNode
readRoadNode s =
    case L.split ' ' s of
      [toid, sp] -> do
        p <- readPoint sp
        return (RN (decodeUtf8 toid) p)
      _ -> Nothing

readPoint :: L.ByteString -> Maybe (Point Double)
readPoint s =
    case L.split ',' s of
      [sx, sy] -> do
        x <- readDouble sx
        y <- readDouble sy
        return (P x y)
      _ -> Nothing

readDouble :: L.ByteString -> Maybe Double
readDouble s =
    case readDecimal (L.toStrict s) of
      Just (d, _) -> Just d
      Nothing -> Nothing
