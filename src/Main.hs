module Main where

import Control.Applicative ((<$>))
import qualified Data.ByteString.Lazy.Char8 as L
import Data.ByteString.Lex.Fractional (readDecimal)
import Data.Maybe (catMaybes)
import System.Directory (createDirectoryIfMissing)

import Geometry.Line
import Geometry.LineClipping.Fast
import Geometry.Point
import Geometry.Polyline
import Options
import Tile
import TileMap


main :: IO ()
main = do
    opts <- getOptions
    ls <- L.lines <$> L.readFile (oPolylines opts)
    ps <- L.lines <$> L.readFile (oPoints opts)
    let out  = oOutput opts
        size = (oWidth opts, oHeight opts)
        tm   = processPoints (processPolylines newTileMap size ls) size ps
    createDirectoryIfMissing True out
    outputTileMap out tm


processPolylines :: TileMap -> (Int, Int) -> [L.ByteString] -> TileMap
processPolylines tm size =
    insertPolylines tm . concatMap (processPolyline size . readPolyline)

processPoints :: TileMap -> (Int, Int) -> [L.ByteString] -> TileMap
processPoints tm size =
    insertPoints tm . map (processPoint size) . catMaybes . map readPoint


processPolyline :: (Int, Int) -> Polyline Double -> [(Polyline Double, (Int, Int))]
processPolyline _ (PL [])  = []
processPolyline _ (PL [_]) = []
processPolyline size (PL (p : ps@(_ : _))) =
    let tc = tileCoords size p
        tb = tileBounds size tc
    in  loop tc tb p ps []
  where
    loop tc _ p1 [] qs = [(PL (reverse (p1 : qs)), tc)]
    loop tc@(tx, ty) tb p1 ps1@(p2 : ps2) qs =
        case reverseFastClipYAxis tb (L p1 p2) of
          (q2, P 0 0)   -> loop tc tb q2 ps2 (p1 : qs)
          (q2, P vx vy) -> let uc = (tx + vx, ty + vy)
                               ub = tileBounds size uc
                               rs = if q2 == p1
                                      then q2 : qs
                                      else q2 : p1 : qs
                           in  (PL (reverse rs), tc) : loop uc ub q2 ps1 []

processPoint :: (Int, Int) -> Point Double -> (Point Double, (Int, Int))
processPoint size p =
    let tc = tileCoords size p
    in  (p, tc)


readPolyline :: L.ByteString -> Polyline Double
readPolyline =
    PL . catMaybes . map readPoint . L.split ' '

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
