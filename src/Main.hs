module Main where

import Control.Monad (forM_)
import qualified Data.ByteString.Lazy.Char8 as L
import Data.ByteString.Lex.Fractional (readDecimal)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as M
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Maybe (catMaybes)
import Options.Applicative ((<$>), (<*>), (<>), Parser)
import qualified Options.Applicative as O
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import System.IO (Handle, IOMode(..), hClose, hPutStrLn, openFile, withFile)


data Options = Options
  { optPolylines :: FilePath
  , optPoints    :: FilePath
  , optWidth     :: Int
  , optHeight    :: Int
  , optOutput    :: FilePath
  }
  deriving (Eq, Show)


main :: IO ()
main = do
    opts <- O.execParser $
      O.info (O.helper <*> parseOptions)
         ( O.header "map-cutter"
        <> O.progDesc "Cut a map composed of polylines and points into tiles"
        <> O.fullDesc
         )
    polylines <- L.lines <$> L.readFile (optPolylines opts)
    points <- L.lines <$> L.readFile (optPoints opts)
    createDirectoryIfMissing True (optOutput opts)
    withFile (optOutput opts </> "polylines.txt") WriteMode $ \h ->
      mapM_ (processPolyline h) polylines
    let out    = optOutput opts
        width  = optWidth opts
        height = optHeight opts
    processPoints out width height points


parseOptions :: Parser Options
parseOptions =
    Options <$>
          parsePolylines
      <*> parsePoints
      <*> parseWidth
      <*> parseHeight
      <*> parseOutput

parsePolylines :: Parser FilePath
parsePolylines =
    O.argument O.str
       ( O.metavar "POLYLINES"
      <> O.help "File containing the polyline part of map input"
       )

parsePoints :: Parser FilePath
parsePoints =
    O.argument O.str
       ( O.metavar "POINTS"
      <> O.help "File containing the point part of map input"
       )

parseWidth :: Parser Int
parseWidth =
    O.option O.auto
       ( O.metavar "WIDTH"
      <> O.short 'w'
      <> O.value 1000
      <> O.help "Width of map tiles to output"
       )

parseHeight :: Parser Int
parseHeight =
    O.option O.auto
       ( O.metavar "HEIGHT"
      <> O.short 'h'
      <> O.value 1000
      <> O.help "Height of map tiles to output"
       )

parseOutput :: Parser FilePath
parseOutput =
    O.strOption
       ( O.metavar "OUTPUT"
      <> O.short 'o'
      <> O.value "dist/out"
      <> O.help "Directory to contain tile output"
       )


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


data TileMap = TM
  { tmOutput :: FilePath
  , tmRef    :: IORef (IntMap (IntMap Handle))
  }

newTileMap :: FilePath -> IO TileMap
newTileMap out = do
    r <- newIORef M.empty
    return (TM out r)

openTile :: TileMap -> Int -> Int -> IO Handle
openTile tm tx ty = do
    xm <- readIORef r
    case M.lookup tx xm of
      Just ym -> case M.lookup ty ym of
        Just h -> return h
        Nothing -> do
          h <- openFile f WriteMode
          let ym' = M.insert ty h ym
              xm' = M.insert tx ym' xm
          writeIORef r xm'
          return h
      Nothing -> do
        h <- openFile f WriteMode
        let xm' = M.insert tx (M.singleton ty h) xm
        writeIORef r xm'
        return h
  where
    r = tmRef tm
    f = tmOutput tm ++ "-" ++ show tx ++ "-" ++ show ty ++ ".txt"

closeAllTiles :: TileMap -> IO ()
closeAllTiles tm = do
    xm <- readIORef r
    forM_ (M.elems xm) $ \ym ->
      forM_ (M.elems ym) $ \h ->
        hClose h
    writeIORef r M.empty
  where
    r = tmRef tm


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
