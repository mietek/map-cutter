module Main where

import Control.Applicative ((<$>))
import Control.Monad (forM_)
import qualified Data.Aeson as J
import qualified Data.ByteString.Lazy.Char8 as L
import Data.ByteString.Lex.Fractional (readDecimal)
import Data.Maybe (catMaybes)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import System.IO (hPutStr, hPutStrLn)

import Geometry.Line
import Geometry.LineClipping.Fast
import Geometry.Point
import Geometry.Rect
import Options
import TileMap


main :: IO ()
main = do
    opts <- getOptions
    polylines <- L.lines <$> L.readFile (oPolylines opts)
    points <- L.lines <$> L.readFile (oPoints opts)
    let out  = oOutput opts
        size = (oWidth opts, oHeight opts)
    createDirectoryIfMissing True out
    processPolylines out size polylines
    processPoints out size points


processPolylines :: FilePath -> (Int, Int) -> [L.ByteString] -> IO ()
processPolylines out size polylines = do
    tm <- newTileMap (out </> "polylines")
    forM_ polylines $ \s -> do
      let ps = readPolyline s
      forM_ (processPolyline size ps) $ \((tx, ty), qs) ->
        openTile tm tx ty $ \h new -> do
          hPutStr h (if new then "[" else ",")
          L.hPutStrLn h (J.encode qs)
    closeAllTiles tm $ \h ->
      hPutStrLn h "]"

processPolyline :: (Int, Int) -> [Point Double] -> [((Int, Int), [Point Double])]
processPolyline _ []  = []
processPolyline _ [_] = []
processPolyline size (p : ps@(_ : _)) =
    let tc = tileCoords size p
        tb = tileBounds size tc
    in  loop tc tb p ps []
  where
    loop tc _ p1 [] qs = [(tc, reverse (p1 : qs))]
    loop tc@(tx, ty) tb p1 ps1@(p2 : ps2) qs =
        case reverseFastClipYAxis tb (L p1 p2) of
          (q2, P 0 0)   -> loop tc tb q2 ps2 (p1 : qs)
          (q2, P vx vy) -> let uc = (tx + vx, ty + vy)
                               ub = tileBounds size uc
                               rs = if q2 == p1
                                      then q2 : qs
                                      else q2 : p1 : qs
                           in  (tc, reverse rs) : loop uc ub q2 ps1 []


processPoints :: FilePath -> (Int, Int) -> [L.ByteString] -> IO ()
processPoints out size points = do
    tm <- newTileMap (out </> "points")
    forM_ points $ \s ->
      case readPoint s of
        Just p -> do
          let (tx, ty) = tileCoords size p
          openTile tm tx ty $ \h new -> do
            hPutStr h (if new then "[" else ",")
            L.hPutStrLn h (J.encode p)
        Nothing -> return ()
    closeAllTiles tm $ \h ->
      hPutStrLn h "]"


tileCoords :: (Int, Int) -> Point Double -> (Int, Int)
tileCoords (width, height) (P x y) = (tx, ty)
  where
    tx = floor (x / fromIntegral width)
    ty = floor (y / fromIntegral height)

tileBounds :: (Int, Int) -> (Int, Int) -> Rect Double
tileBounds (width, height) (tx, ty) = R left top right bottom
  where
    left   = fromIntegral (width  * tx)
    top    = fromIntegral (height * ty)
    right  = fromIntegral (width  * (tx + 1))
    bottom = fromIntegral (height * (ty + 1))


readPolyline :: L.ByteString -> [Point Double]
readPolyline = catMaybes . map readPoint . L.split ' '

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
