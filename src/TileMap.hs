module TileMap where

import Control.Monad (forM_)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as M
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import System.IO (Handle, IOMode(..), hClose, openFile)


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
      forM_ (M.elems ym) $ \h -> do
        hClose h
    writeIORef r M.empty
  where
    r = tmRef tm
