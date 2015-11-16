{-# LANGUAGE Arrows #-}
module Collision (collision) where

import Prelude hiding ((.),)
import Control.Wire
import Control.Monad.IO.Class()
import FRP.Netwire()
import Linear.V2
import Linear.Matrix
import Configure


collision :: (HasTime t s) => Wire s () IO (Ball, Ball) NextS
collision = proc (b1@(Ball _ _ r1 (V2 vx1 vy1) (V2 x1 y1) (V2 dt1 dt1')), b2@(Ball _ _ r2 (V2 vx2 vy2) (V2 x2 y2) (V2 dt2 dt2'))) -> do
    let nx1 = x1 + vx1 * dt1
        nx2 = x2 + vx2 * dt2
        ny1 = y1 + vy1 * dt1'
        ny2 = y2 + vy2 * dt2'
        v1  = nx1 - r1 < (- wallV) || nx1 + r1 > wallV
        v2  = nx2 - r2 < (- wallV) || nx2 + r2 > wallV
        h1  = ny1 - r1 < (- wallH) || ny1 + r1 > wallH
        h2  = ny2 - r2 < (- wallH) || ny2 + r2 > wallH
        bC = collision1 b1 b2
    returnA -< NextS bC b1 b2 (WallC v1 v2 h1 h2)

collision1 :: Ball -> Ball -> BallC
collision1 (Ball n1 m1 r1 v1@(V2 vx1 vy1) p1@(V2 px1 py1) step1@(V2 dt1 dt1')) (Ball n2 m2 r2 v2@(V2 vx2 vy2) p2@(V2 px2 py2) step2@(V2 dt2 dt2')) = 
  if dx*dx + dy*dy > r*r then BallC False n1 v1 nxt1 n2 v2 nxt2 else BallC True n1 v1' p1' n2 v2' p2'
  where
      m          = m1 + m2
      invm       = 1 / m
      r          = r1 + r2
      invr       = 1 / r
-- Position and velocity of the gravity center
      p          = (V2 (V2 px1 px2) (V2 py1 py2) !* V2 m1 m2) * V2 invm invm
      v@(V2 _ vy)= (V2 (V2 vx1 vx2) (V2 vy1 vy2) !* V2 m1 m2) * V2 invm invm
-- Transportation matrix for the rotation coordinate system
      V2 dvx dvy = v1 - v2
      dv         = sqrt $ dvx * dvx + dvy * dvy
      V2 cost sint = if dvy > 0 then V2 (dvx / dv) (dvy / dv) else V2 (-dvx / dv) (-dvy / dv)
      l          = abs $ cost * (py1 - py2) - sint * (px1 - px2)
      lr         = l * invr
      sqlr       = sqrt (r * r - l * l) * invr

      nxt@(V2 nxtx nxty) = nxt1 - p
      cur@(V2 curx cury) = p1 - p
      V2 cosp sinp  
        | curx*nxty - nxtx*cury <0  = V2 (lr * cost - sqlr * sint)  (sqlr * cost + lr * sint)    -- clockwise
        | otherwise                 = V2 (lr * cost + sqlr * sint)  (- sqlr * cost + lr * sint)

      mat        = V2 (V2 cosp sinp) (V2 (-sinp) cosp)

      V2 a1 b1   = mat !* (p1 - p)
      V2 a2 b2   = mat !* (p2 - p)
      V2 ua1 ub1 = mat !* (v1 - v)
      V2 ua2 ub2 = mat !* (v2 - v)
-- Position and velocity immediately after the collision on the rotation coordinate

      d          = (- m1 * r1 + m2 * r2) / m
      (s1',s2')
        | b1 - b2 > 0 = (V2 (a1 + dt1 * ua1) ( 2*(d + r1) - (b1 + dt1' * ub1)), V2 (a2 + dt2 * ua2) ( 2*(d - r2) - (b2 + dt2' * ub2)))
        | otherwise   = (V2 (a1 + dt1 * ua1) (-2*(d + r1) - (b1 + dt1' * ub1)), V2 (a2 + dt2 * ua2) (-2*(d - r2) - (b2 + dt2' * ub2)))
      (u1',u2')     = (V2 ua1 (-ub1), V2 ua2 (-ub2))

-- Transform to the billiard coodinate system
      invmat     = V2 (V2 cosp (-sinp)) (V2 sinp cosp)
      p1'        = invmat !* s1' + p
      p2'        = invmat !* s2' + p
      v1'        = invmat !* u1' + v
      v2'        = invmat !* u2' + v
-- Difference between two balls at next step      
      nxt1       = p1 + v1 * step1
      nxt2       = p2 + v2 * step2
      V2 dx dy   = nxt1 - nxt2

collision2 =
  collision2' (Ball "ball1" mass1 radius1 vInit1 pInit1 (V2 1.0e-3 1.0e-3)) (Ball "ball2" mass2 radius2 vInit2 pInit2 (V2 1.0e-3 1.0e-3))

collision2' :: Ball -> Ball -> IO()
collision2' ball1@(Ball n1 m1 r1 v1@(V2 vx1 vy1) p1@(V2 px1 py1) step1@(V2 dt1 dt1')) ball2@(Ball n2 m2 r2 v2@(V2 vx2 vy2) p2@(V2 px2 py2) step2@(V2 dt2 dt2')) = do
  print $ "ball1: " ++ (show ball1) ++ "ball2: " ++ (show ball2)
  if dx*dx + dy*dy > r*r 
    then 
      print $ "No Collision: distance " ++ (show  $ sqrt (dx*dx + dy*dy)) ++ " must be smaller than " ++ (show r)
    else do
      print $ BallC True n1 v1' p1' n2 v2' p2'
      print $ "p: " ++ (show p) ++ "v: " ++ (show v)
      print $ "cost:" ++ (show cost) ++ " sint:" ++ (show sint) ++" l:" ++ (show l)
      print $ "cur:" ++ (show cur) ++ " nxt:" ++ (show nxt)
      print $ "cosp:" ++ (show cosp) ++ " sinp:" ++ (show sinp)
      print $ "a1:" ++ (show a1) ++ " b1:" ++ (show b1) ++ " a2:" ++ (show a2) ++ " b2:" ++ (show b2)
      print $ "ua1:" ++ (show ua1) ++ " ub1:" ++ (show ub1) ++ " ua2:" ++ (show ua2) ++ " ub2:"++ (show ub2)
      print $ "s1':" ++ (show s1') ++ " s2':" ++ (show s2')
      print $ "u1':" ++ (show u1') ++ " u2':" ++ (show u2')
      print $ "p1':" ++ (show p1') ++ " p2':" ++ (show p2')
      print $ "v1':" ++ (show v1') ++ " p2':" ++ (show v2')
  where
      m          = m1 + m2
      invm       = 1 / m
      r          = r1 + r2
      invr       = 1 / r
-- Position and velocity of the gravity center
      p          = (V2 (V2 px1 px2) (V2 py1 py2) !* V2 m1 m2) * V2 invm invm
      v@(V2 _ vy)= (V2 (V2 vx1 vx2) (V2 vy1 vy2) !* V2 m1 m2) * V2 invm invm
-- Transportation matrix for the rotation coordinate system
      V2 dvx dvy = v1 - v2
      dv         = sqrt $ dvx * dvx + dvy * dvy
      V2 cost sint = if dvy > 0 then V2 (dvx / dv) (dvy / dv) else V2 (-dvx / dv) (-dvy / dv)
      l          = abs $ cost * (py1 - py2) - sint * (px1 - px2)
      lr         = l * invr
      sqlr       = sqrt (r * r - l * l) * invr

      nxt@(V2 nxtx nxty) = nxt1 - p
      cur@(V2 curx cury) = p1 - p
      V2 cosp sinp  
        | curx*nxty - nxtx*cury > 0 = V2 (lr * cost - sqlr * sint)  (sqlr * cost + lr * sint)    -- clockwise
        | otherwise                 = V2 (lr * cost + sqlr * sint)  (- sqlr * cost + lr * sint)

      mat        = V2 (V2 cosp sinp) (V2 (-sinp) cosp)

      V2 a1 b1   = mat !* (p1 - p)
      V2 a2 b2   = mat !* (p2 - p)
      V2 ua1 ub1 = mat !* (v1 - v)
      V2 ua2 ub2 = mat !* (v2 - v)
-- Position and velocity immediately after the collision on the rotation coordinate

      d          = (- m1 * r1 + m2 * r2) / m
      (s1',s2')
        | b1 - b2 > 0 = (V2 (a1 + dt1 * ua1) ( 2*(d + r1) - (b1 + dt1' * ub1)), V2 (a2 + dt2 * ua2) ( 2*(d - r2) - (b2 + dt2' * ub2)))
        | otherwise   = (V2 (a1 + dt1 * ua1) (-2*(d + r1) - (b1 + dt1' * ub1)), V2 (a2 + dt2 * ua2) (-2*(d - r2) - (b2 + dt2' * ub2)))
      (u1',u2')     = (V2 ua1 (-ub1), V2 ua2 (-ub2))

-- Transform to the billiard coodinate system
      invmat     = V2 (V2 cosp (-sinp)) (V2 sinp cosp)
      p1'        = invmat !* s1' + p
      p2'        = invmat !* s2' + p
      v1'        = invmat !* u1' + v
      v2'        = invmat !* u2' + v
-- Difference between two balls at next step      
      nxt1       = p1 + v1 * step1
      nxt2       = p2 + v2 * step2
      V2 dx dy   = nxt1 - nxt2
