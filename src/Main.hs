module Main where

import Control.Applicative ((<$>))
import Control.Monad (forM_)
import qualified Data.ByteString.Lazy.Char8 as L
import Data.ByteString.Lex.Fractional (readDecimal)
import Data.Maybe (catMaybes)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import System.IO (Handle, IOMode(..), hPutStrLn, withFile)

import Options
import TileMap


main :: IO ()
main = do
    opts <- getOptions
    polylines <- L.lines <$> L.readFile (oPolylines opts)
    points <- L.lines <$> L.readFile (oPoints opts)
    createDirectoryIfMissing True (oOutput opts)
    withFile (oOutput opts </> "polylines.txt") WriteMode $ \h ->
      mapM_ (processPolyline h) polylines
    processPoints (oOutput opts) (oWidth opts) (oHeight opts) points


processPolyline :: Handle -> L.ByteString -> IO ()
processPolyline h s =
    case readPolyline s of
      [] -> return ()
      ps -> hPutStrLn h (showPolyline ps)

processPoints :: FilePath -> Int -> Int -> [L.ByteString] -> IO ()
processPoints out width height points = do
    tm <- newTileMap (out </> "points")
    forM_ points $ \s ->
      case readPoint s of
        Just p@(x, y) -> do
          h <- openTile tm (floor x `div` width) (floor y `div` height)
          hPutStrLn h (showPoint p)
        Nothing -> return ()
    closeAllTiles tm


readPolyline :: L.ByteString -> [(Double, Double)]
readPolyline = catMaybes . map readPoint . L.split ' '

readPoint :: L.ByteString -> Maybe (Double, Double)
readPoint s =
    case L.split ',' s of
      [sx, sy] -> do
        x <- readDouble sx
        y <- readDouble sy
        return (x, y)
      _ -> Nothing

readDouble :: L.ByteString -> Maybe Double
readDouble s =
    case readDecimal (L.toStrict s) of
      Just (d, _) -> Just d
      Nothing -> Nothing


showPolyline :: [(Double, Double)] -> String
showPolyline = unwords . map showPoint

showPoint :: (Double, Double) -> String
showPoint (x, y) = show x ++ "," ++ show y
