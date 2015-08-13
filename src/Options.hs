module Options where

import Options.Applicative ((<$>), (<*>), (<>), Parser)
import qualified Options.Applicative as O


data Options = Options
  { oPolylines :: FilePath
  , oPoints    :: FilePath
  , oWidth     :: Int
  , oHeight    :: Int
  , oOutput    :: FilePath
  }
  deriving (Eq, Show)


getOptions :: IO Options
getOptions =
    O.execParser $
      O.info (O.helper <*> parseOptions)
         ( O.header "map-cutter"
        <> O.progDesc "Cut a map composed of polylines and points into tiles"
        <> O.fullDesc
         )

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
