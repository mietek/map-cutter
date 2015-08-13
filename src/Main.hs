{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative ((<$>))
import Control.Monad (forM_)
import Data.Aeson ((.=), ToJSON, toJSON)
import qualified Data.Aeson as J
import qualified Data.ByteString.Lazy.Char8 as L
import Data.ByteString.Lex.Fractional (readDecimal)
import Data.Maybe (catMaybes)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import System.IO (Handle, IOMode(..), hPutStr, hPutStrLn, withFile)

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


data Point = P Double Double
  deriving (Eq, Ord, Show)

instance ToJSON Point where
  toJSON (P x y) = J.object ["x" .= x, "y" .= y]


processPoints :: FilePath -> Int -> Int -> [L.ByteString] -> IO ()
processPoints out width height points = do
    tm <- newTileMap (out </> "points")
    forM_ points $ \s ->
      case readPoint s of
        Just p@(P x y) -> do
          let tx = floor x `div` width
              ty = floor y `div` height
          openTile tm tx ty $ \h new -> do
            hPutStr h (if new then "[" else ",")
            L.hPutStrLn h (J.encode p)
        Nothing -> return ()
    closeAllTiles tm $ \h ->
      hPutStrLn h "]"


readPolyline :: L.ByteString -> [Point]
readPolyline = catMaybes . map readPoint . L.split ' '

readPoint :: L.ByteString -> Maybe Point
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


showPolyline :: [Point] -> String
showPolyline = unwords . map showPoint

showPoint :: Point -> String
showPoint (P x y) = show x ++ "," ++ show y
