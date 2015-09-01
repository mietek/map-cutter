module Options where

import Options.Applicative ((<$>), (<*>), (<>), Parser)
import qualified Options.Applicative as O


data Options = Options
  { oRoadLinks :: FilePath
  , oRoadNodes :: FilePath
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
        <> O.progDesc "Cut an OS map into tiles"
        <> O.fullDesc
         )

parseOptions :: Parser Options
parseOptions =
    Options <$>
          parseRoadLinks
      <*> parseRoadNodes
      <*> parseWidth
      <*> parseHeight
      <*> parseOutput

parseRoadLinks :: Parser FilePath
parseRoadLinks =
    O.argument O.str
       ( O.metavar "ROADLINKS"
      <> O.help "File containing OS RoadLink input"
       )

parseRoadNodes :: Parser FilePath
parseRoadNodes =
    O.argument O.str
       ( O.metavar "ROADNODES"
      <> O.help "File containing OS RoadNode input"
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
      <> O.help "Output directory"
       )
