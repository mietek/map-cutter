{-

T.M. Nicholl, D.T. Lee, R.A. Nicholl, “An efficient new algorithm for 2D line clipping: Its development and analysis”, Computer Graphics, volume 21, number 4, July 1987

 left top      | centre top    | right top
---------------+---------------+---------------
 left middle   | centre middle | right middle
---------------+---------------+---------------
 left bottom   | centre bottom | right bottom

-}

module Geometry.LineClipping.Fast (fastClip, reverseFastClipYAxis) where

import Geometry.Line
import Geometry.LineClipping.Common
import Geometry.Point
import Geometry.Rect


-- Clip line to rect
-- Returns clipped P2 and unit vector pointing to the clip region
-- Assumes y axis is pointing up and P1 is in the centre-middle region
fastClip :: (RealFrac a) => Rect a -> Line a -> (Point a, Point Int)
fastClip r l = _p1CentreMiddle r l

-- Clip line to rect
-- Returns clipped P2 and unit vector pointing to the clip region
-- Assumes y axis is pointing down and P1 is in the centre-middle region
reverseFastClipYAxis :: (RealFrac a) => Rect a -> Line a -> (Point a, Point Int)
reverseFastClipYAxis r l = let (q2, u) = _p1CentreMiddle (reverseRectYAxis r) l
                           in (q2, rotatePoint180c u)


-- 2.1. "inside"
-- P1 is in the centre-middle region
_p1CentreMiddle :: (RealFrac a) => Rect a -> Line a -> (Point a, Point Int)
_p1CentreMiddle r@(R left top right bottom) l@(L _ p2@(P p2x p2y))
  | p2x < left   = _p1CentreMiddle_p2Left r l
  | p2x > right  = let (q2, u) = _p1CentreMiddle_p2Left (rotateRect180c r) (rotateLine180c l)
                   in (rotatePoint180c q2, rotatePoint180c u)
  | p2y > top    = (clipTop r l d topP, P 0 (-1))
  | p2y < bottom = (clipBottom r l d bottomP, P 0 1)
  | otherwise    = (p2, P 0 0)
  where
    d       = delta l
    topP    = topProduct r l d
    bottomP = bottomProduct r l d

-- P1 is in the centre-middle region, and P2 is in one of the left regions
_p1CentreMiddle_p2Left :: (RealFrac a) => Rect a -> Line a -> (Point a, Point Int)
_p1CentreMiddle_p2Left r@(R _ top _ bottom) l@(L _ (P _ p2y))
  | p2y > top    = _p1CentreMiddle_p2LeftTop r l
  | p2y < bottom = let (q2, u) = _p1CentreMiddle_p2LeftTop (rotateRect90c r) (rotateLine90c l)
                   in (rotatePoint270c q2, rotatePoint270c u)
  | otherwise    = (clipLeft r l d leftP, P 1 0)
  where
    d     = delta l
    leftP = leftProduct r l d

-- P1 is in the centre-middle region, and P2 is in the left-top region
_p1CentreMiddle_p2LeftTop :: (RealFrac a) => Rect a -> Line a -> (Point a, Point Int)
_p1CentreMiddle_p2LeftTop r l
  | topP > leftP = (clipTop r l d topP, P 0 (-1))
  | otherwise    = (clipLeft r l d leftP, P 1 0)
  where
    d     = delta l
    leftP = leftProduct r l d
    topP  = topProduct r l d
