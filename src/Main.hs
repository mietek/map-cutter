module Main where

import qualified Data.ByteString.Lazy.Char8 as L
import Data.ByteString.Lex.Fractional (readDecimal)
import Data.Maybe (catMaybes)
import Options.Applicative ((<$>), (<*>), (<>), Parser)
import qualified Options.Applicative as O
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import System.IO (Handle, IOMode(..), hPutStrLn, withFile)


data Options = Options
  { optPolylines :: FilePath
  , optPoints    :: FilePath
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
    withFile (optOutput opts </> "points.txt") WriteMode $ \h ->
      mapM_ (processPoint h) points


parseOptions :: Parser Options
parseOptions =
    Options <$>
          parsePolylines
      <*> parsePoints
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

processPoint :: Handle -> L.ByteString -> IO ()
processPoint h s =
    case readPoint s of
      Nothing -> return ()
      Just p -> hPutStrLn h (showPoint p)


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
