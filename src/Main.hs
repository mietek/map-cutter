module Main where

import qualified Data.ByteString.Lazy.Char8 as L
import Data.ByteString.Lex.Fractional (readDecimal)
import qualified Data.HashMap.Strict as M
import Data.List (zipWith5)
import Data.Maybe (catMaybes, fromJust)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
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
import TileGroupMap


main :: IO ()
main = do
    opts <- getOptions
    ls <- L.lines <$> L.readFile (oRoadLinks opts)
    ns <- L.lines <$> L.readFile (oRoadNodes opts)
    let out  = oOutput opts
        size = (oWidth opts, oHeight opts)
        tm   = processRoadNodes (processRoadLinks newTileMap size ls) size ns
        tgm  = processTiles newTileGroupMap tm
    createDirectoryIfMissing True out
    outputTileGroupMap out tgm


processTiles :: TileGroupMap -> TileMap -> TileGroupMap
processTiles tgm tm =
    insertTiles tgm (zip tcs ts'')
  where
    (tcs, ts) = unzip (M.toList tm)
    ls    = map (map rlLength . tRoadLinks) ts
    lmins = map minimum ls
    lmaxs = map maximum ls
    (lmeans, lstddevs) = unzip (map meanStdDev ls)
    gs   = concat ls
    gmin = minimum gs
    gmax = maximum gs
    (gmean, gstddev) = meanStdDev gs
    ts'  = zipWith5 setLocalData lmins lmaxs lmeans lstddevs ts
    ts'' = map (setGlobalData gmin gmax gmean gstddev) ts'

mean :: [Double] -> Double
mean xs = sum xs / fromIntegral (length xs)

meanStdDev :: [Double] -> (Double, Double)
meanStdDev xs =
    let m   = mean xs
        d x = let y = x - m in y * y
        v   = mean (map d xs)
        sd  = sqrt v
    in  (m, sd)


processRoadLinks :: TileMap -> (Int, Int) -> [L.ByteString] -> TileMap
processRoadLinks tm size =
    insertRoadLinks tm . concatMap (processRoadLink size) . catMaybes . map readRoadLink

processRoadNodes :: TileMap -> (Int, Int) -> [L.ByteString] -> TileMap
processRoadNodes tm size =
    insertRoadNodes tm . map (processRoadNode size) . catMaybes . map readRoadNode


replacePoints :: RoadLink -> Polyline Double -> RoadLink
replacePoints rl ps =
    rl { rlPoints = ps }

processRoadLink :: (Int, Int) -> RoadLink -> [(RoadLink, (Int, Int))]
processRoadLink size rl =
    case rlPoints rl of
      PL []  -> []
      PL [_] -> []
      PL (p : ps@(_ : _)) ->
        let tc = tileCoords size p
            tb = tileBounds size tc
        in  loop tc tb p ps []
  where
    loop tc _ p1 [] qs =
        [(replacePoints rl (PL (reverse (p1 : qs))), tc)]
    loop tc@(tx, ty) tb p1 ps1@(p2 : ps2) qs =
        case reverseFastClipYAxis tb (L p1 p2) of
          (q2, P 0 0) ->
            loop tc tb q2 ps2 (p1 : qs)
          (q2, P vx vy) ->
            let uc = (tx + vx, ty + vy)
                ub = tileBounds size uc
                rs = if q2 == p1
                       then q2 : qs
                       else q2 : p1 : qs
            in  (replacePoints rl (PL (reverse rs)), tc) : loop uc ub q2 ps1 []

processRoadNode :: (Int, Int) -> RoadNode -> (RoadNode, (Int, Int))
processRoadNode size rn =
    let tc = tileCoords size (rnPoint rn)
    in  (rn, tc)


readRoadLink :: L.ByteString -> Maybe RoadLink
readRoadLink s =
    case L.split ' ' s of
      (toid : sl : rest) -> do
          let (_ : sn1 : sn2 : rsp) = reverse rest
              n1 = decodeUtf8 sn1
              n2 = decodeUtf8 sn2
              ps = readPolyline (reverse rsp)
          l <- readDouble sl
          return $ RL
            { rlTOID    = decodeUtf8 toid
            , rlPoints  = ps
            , rlLength  = l
            , rlNegNode = fromJust (selectNegNode n1 n2)
            , rlPosNode = fromJust (selectPosNode n1 n2)
            }
      _ -> Nothing

selectNegNode :: Text -> Text -> Maybe Text
selectNegNode n1 n2
  | T.head n1 == '-' && T.head n2 == '+' = Just (T.tail n1)
  | T.head n1 == '+' && T.head n2 == '-' = Just (T.tail n2)
  | otherwise                            = Nothing

selectPosNode :: Text -> Text -> Maybe Text
selectPosNode n1 n2
  | T.head n1 == '-' && T.head n2 == '+' = Just (T.tail n2)
  | T.head n1 == '+' && T.head n2 == '-' = Just (T.tail n1)
  | otherwise                            = Nothing

readPolyline :: [L.ByteString] -> Polyline Double
readPolyline = PL . catMaybes . map readPoint

readRoadNode :: L.ByteString -> Maybe RoadNode
readRoadNode s =
    case L.split ' ' s of
      [toid, sp] -> do
        p <- readPoint sp
        return $ RN
          { rnTOID  = decodeUtf8 toid
          , rnPoint = p
          }
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
