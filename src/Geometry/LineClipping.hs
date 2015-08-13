{-

T.M. Nicholl, D.T. Lee, R.A. Nicholl, “An efficient new algorithm for 2D line clipping: Its development and analysis”, Computer Graphics, volume 21, number 4, July 1987

 left top      | centre top    | right top
---------------+---------------+---------------
 left middle   | centre middle | right middle
---------------+---------------+---------------
 left bottom   | centre bottom | right bottom

-}

module Geometry.LineClipping (clip, reverseClipYAxis) where

import Control.Applicative ((<$>))

import Geometry.Line
import Geometry.LineClipping.Common
import Geometry.Point
import Geometry.Rect


-- Clip line to rect
-- Assumes y axis is pointing up
clip :: (RealFrac a) => Rect a -> Line a -> Maybe (Line a)
clip r@(R left _ right _) l@(L (P p1x _) _)
  | p1x < left  = _p1Left r l
  | p1x > right = rotateLine180c <$> _p1Left (rotateRect180c r) (rotateLine180c l)
  | otherwise   = _p1Centre r l

-- Clip line to rect
-- Assumes y axis is pointing down
reverseClipYAxis :: (RealFrac a) => Rect a -> Line a -> Maybe (Line a)
reverseClipYAxis r l = clip (reverseRectYAxis r) l


-- 1. "leftcolumn"
-- P1 is in one of the left regions
_p1Left :: (RealFrac a) => Rect a -> Line a -> Maybe (Line a)
_p1Left r@(R left top _ bottom) l@(L (P _ p1y) (P p2x _))
  | p2x < left   = Nothing
  | p1y > top    = _p1LeftTop_p2NotLeft r l
  | p1y < bottom = reflectLineXAxis <$> _p1LeftTop_p2NotLeft (reflectRectXAxis r) (reflectLineXAxis l)
  | otherwise    = _p1LeftMiddle_p2NotLeft r l


-- 1.1. "topleftcorner"
-- P1 is in the left-top region, and P2 is not in any of the left regions
_p1LeftTop_p2NotLeft :: (RealFrac a) => Rect a -> Line a -> Maybe (Line a)
_p1LeftTop_p2NotLeft r@(R _ top _ _) l@(L _ (P _ p2y))
  | p2y > top = Nothing
  | otherwise = _p1LeftTop_p2NotLeftTop r l d
  where
    d = delta l

-- P1 is in the left-top region, and P2 is not in any of the left or top regions
_p1LeftTop_p2NotLeftTop :: (RealFrac a) => Rect a -> Line a -> Point a -> Maybe (Line a)
_p1LeftTop_p2NotLeftTop r l d
  | topP > leftP = _p1LeftTop_p2NotLeftTop' r l d leftP
  | otherwise    = reflectLineXMinusY <$> _p1LeftTop_p2NotLeftTop' (reflectRectXMinusY r) (reflectLineXMinusY l) (reflectPointXMinusY d) topP
  where
    topP  = topProduct r l d
    leftP = leftProduct r l d


-- 1.1.1. "leftbottomregion"
-- P1 is in the left-top region, and P2 is not in any of the left or top regions, and above the vector from P1 to the left-top corner
_p1LeftTop_p2NotLeftTop' :: (RealFrac a) => Rect a -> Line a -> Point a -> a -> Maybe (Line a)
_p1LeftTop_p2NotLeftTop' r@(R _ _ _ bottom) l@(L _ (P _ p2y)) d leftP
  | p2y < bottom = _p1LeftTop_p2Bottom r l d leftP
  | otherwise    = Just (L (clipLeft r l d leftP) (_p1LeftTop_p2Middle r l d))

-- P1 is in the left-top region, and P2 is the centre-middle or right-middle region
_p1LeftTop_p2Middle :: (RealFrac a) => Rect a -> Line a -> Point a -> Point a
_p1LeftTop_p2Middle r@(R _ _ right _) l@(L _ p2@(P p2x _)) d
  | p2x > right = clipRight r l d rightP
  | otherwise   = p2
  where
    rightP = rightProduct r l d

-- P1 is in the left-top region, and P2 is in the centre-bottom or right-bottom region
_p1LeftTop_p2Bottom :: (RealFrac a) => Rect a -> Line a -> Point a -> a -> Maybe (Line a)
_p1LeftTop_p2Bottom r l d leftP
  | bottomP > leftP = Nothing
  | otherwise       = Just (L (clipLeft r l d leftP) (_p1LeftTop_p2Bottom' r l d bottomP))
  where
    bottomP = bottomProduct r l d

_p1LeftTop_p2Bottom' :: (RealFrac a) => Rect a -> Line a -> Point a -> a -> Point a
_p1LeftTop_p2Bottom' r@(R _ _ right _) l@(L _ (P p2x _)) d bottomP
  | p2x > right = _p1LeftTop_p2BottomRight r l d bottomP
  | otherwise   = clipBottom r l d bottomP

-- P1 is in the left-top region, and P2 is in the right-bottom region
_p1LeftTop_p2BottomRight :: (RealFrac a) => Rect a -> Line a -> Point a -> a -> Point a
_p1LeftTop_p2BottomRight r l d bottomP
  | bottomP > rightP = clipBottom r l d bottomP
  | otherwise        = clipRight r l d rightP
  where
    rightP = rightProduct r l d


-- 1.2. "leftedge"
-- P1 is in the left-middle region, and P2 is not in any of the left regions
_p1LeftMiddle_p2NotLeft :: (RealFrac a) => Rect a -> Line a -> Maybe (Line a)
_p1LeftMiddle_p2NotLeft r@(R _ top _ bottom) l@(L _ (P _ p2y))
  | p2y < bottom = _p1LeftMiddle_p2BottomNotLeft r l
  | p2y > top    = reflectLineXAxis <$> _p1LeftMiddle_p2BottomNotLeft (reflectRectXAxis r) (reflectLineXAxis l)
  | otherwise    = Just (L (clipLeft r l d leftP) (_p1LeftMiddle_p2MiddleNotLeft r l d))
  where
    d     = delta l
    leftP = leftProduct r l d

-- P1 is in the left-middle region, and P2 is the centre-middle or right-middle region
_p1LeftMiddle_p2MiddleNotLeft :: (RealFrac a) => Rect a -> Line a -> Point a -> Point a
_p1LeftMiddle_p2MiddleNotLeft r@(R _ _ right _) l@(L _ p2@(P p2x _)) d
  | p2x > right = clipRight r l d rightP
  | otherwise   = p2
  where
    rightP = rightProduct r l d


-- 1.2.1. "p2bottom"
-- P1 is in the left-middle region, and P2 is in the centre-bottom or right-bottom region
_p1LeftMiddle_p2BottomNotLeft :: (RealFrac a) => Rect a -> Line a -> Maybe (Line a)
_p1LeftMiddle_p2BottomNotLeft r l
  | bottomP > leftP = Nothing
  | otherwise       = Just (L (clipLeft r l d leftP) (_p1LeftMiddle_p2BottomNotLeft' r l d bottomP))
  where
    d       = delta l
    leftP   = leftProduct r l d
    bottomP = bottomProduct r l d

_p1LeftMiddle_p2BottomNotLeft' :: (RealFrac a) => Rect a -> Line a -> Point a -> a -> Point a
_p1LeftMiddle_p2BottomNotLeft' r@(R _ _ right _) l@(L _ (P p2x _)) d bottomP
  | p2x > right = _p1LeftMiddle_p2RightBottom r l d bottomP
  | otherwise   = clipBottom r l d bottomP

-- P2 is beyond the right boundary
_p1LeftMiddle_p2RightBottom :: (RealFrac a) => Rect a -> Line a -> Point a -> a -> Point a
_p1LeftMiddle_p2RightBottom r l d bottomP
  | bottomP > rightP = clipBottom r l d bottomP
  | otherwise        = clipRight r l d rightP
  where
    rightP = rightProduct r l d


-- 2. "centrecolumn"
-- P1 is in one of the centre regions
_p1Centre :: (RealFrac a) => Rect a -> Line a -> Maybe (Line a)
_p1Centre r@(R _ top _ bottom) l@(L p1@(P _ p1y) _)
  | p1y < bottom = _p1CentreBottom r l
  | p1y > top    = _p1CentreTop r l
  | otherwise    = Just (L p1 (_p1CentreMiddle r l))

-- P1 is in the centre-bottom region
_p1CentreBottom :: (RealFrac a) => Rect a -> Line a -> Maybe (Line a)
_p1CentreBottom r@(R _ _ _ bottom) l@(L _ (P _ p2y))
  | p2y < bottom = Nothing
  | otherwise    = rotateLine270c <$> _p1LeftMiddle_p2NotLeft (rotateRect90c r) (rotateLine90c l)

-- P1 is in the centre-top region
_p1CentreTop :: (RealFrac a) => Rect a -> Line a -> Maybe (Line a)
_p1CentreTop r@(R _ top _ _) l@(L _ (P _ p2y))
  | p2y > top = Nothing
  | otherwise = rotateLine90c <$> _p1LeftMiddle_p2NotLeft (rotateRect270c r) (rotateLine270c l)


-- 2.1. "inside"
-- P1 is in the centre-middle region
_p1CentreMiddle :: (RealFrac a) => Rect a -> Line a -> Point a
_p1CentreMiddle r@(R left top right bottom) l@(L _ p2@(P p2x p2y))
  | p2x < left   = _p1CentreMiddle_p2Left r l
  | p2x > right  = rotatePoint180c $ _p1CentreMiddle_p2Left (rotateRect180c r) (rotateLine180c l)
  | p2y > top    = clipTop r l d topP
  | p2y < bottom = clipBottom r l d bottomP
  | otherwise    = p2
  where
    d       = delta l
    topP    = topProduct r l d
    bottomP = bottomProduct r l d

-- P1 is in the centre-middle region, and P2 is in one of the left regions
_p1CentreMiddle_p2Left :: (RealFrac a) => Rect a -> Line a -> Point a
_p1CentreMiddle_p2Left r@(R _ top _ bottom) l@(L _ (P _ p2y))
  | p2y > top    = _p1CentreMiddle_p2LeftTop r l
  | p2y < bottom = rotatePoint270c $ _p1CentreMiddle_p2LeftTop (rotateRect90c r) (rotateLine90c l)
  | otherwise    = clipLeft r l d leftP
  where
    d     = delta l
    leftP = leftProduct r l d

-- P1 is in the centre-middle region, and P2 is in the left-top region
_p1CentreMiddle_p2LeftTop :: (RealFrac a) => Rect a -> Line a -> Point a
_p1CentreMiddle_p2LeftTop r l
  | topP > leftP = clipTop r l d topP
  | otherwise    = clipLeft r l d leftP
  where
    d     = delta l
    leftP = leftProduct r l d
    topP  = topProduct r l d
