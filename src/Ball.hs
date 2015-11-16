{-# LANGUAGE Arrows #-}
module Ball (ball) where

import Prelude hiding ((.),)
import Control.Wire
import FRP.Netwire.Move()
import Linear.V2
import Configure
import Collision()

velocity :: (HasTime t s, Monad m) => V2 Double ->  Wire s () m (String, NextS) (V2 Double) 
velocity = constWith collide
  where
    collide (self, NextS (BallC b nam1 (V2 nxtVx1 nxtVy1) _ _ (V2 nxtVx2 nxtVy2) _) (Ball n1 _ _ (V2 vx1 vy1) (V2 _ _) (V2 _ _)) (Ball n2 _ _ (V2 vx2 vy2) (V2 _ _) (V2 _ _)) (WallC v1 v2 h1 h2))
            | v1 && self == n1    = V2 (-vx1) vy1
            | v2 && self == n2    = V2 (-vx2) vy2
            | h1 && self == n1    = V2 vx1 (-vy1)
            | h2 && self == n2    = V2 vx2 (-vy2)
            | b && self  == nam1  = V2 nxtVx1 nxtVy1
            | b                   = V2 nxtVx2 nxtVy2
            | self == n1          = V2 vx1 vy1
            | otherwise           = V2 vx2 vy2

constWith :: (Fractional a, HasTime t s)=> (w -> a) -> a -> Wire s e m w a
constWith correct = loop'
  where
    loop' x' = mkPure $ \_ w ->
      let x  = correct w
      in x' `seq` (Right x', loop' x)


position :: (HasTime t s, Monad m) => V2 Double -> Wire s () m (V2 Double, String, NextS) (V2 Double, V2 Double)
position = integralWith' collide'
  where 
    collide' self (NextS (BallC b nam1 _ (V2 nxtPx1 nxtPy1) _ _ (V2 nxtPx2 nxtPy2)) (Ball n1 _ r1 (V2 vx1 vy1) (V2 x1 y1) (V2 dt1 dt1')) (Ball n2 _ r2 (V2 vx2 vy2) (V2 x2 y2) (V2 dt2 dt2')) (WallC v1 v2 h1 h2)) pos
            | v1 && self == n1   = V2 (if vx1 > 0 then 2*wallV - (x1+dt1*vx1) - 2 * r1 else 2*(-wallV) - (x1+dt1*vx1) + 2 * r1) y1
            | v2 && self == n2   = V2 (if vx2 > 0 then 2*wallV - (x2+dt2*vx2) - 2 * r2 else 2*(-wallV) - (x2+dt2*vx2) + 2 * r2) y2 
            | h1 && self == n1   = V2 x1 (if vy1 > 0 then 2*wallH - (y1+dt1'*vy1) - 2 * r1 else 2*(-wallH) - (y1+dt1'*vy1) + 2 * r1)
            | h2 && self == n2   = V2 x2 (if vy2 > 0 then 2*wallH - (y2+dt2'*vy2) - 2 * r2 else 2*(-wallH) - (y2+dt2'*vy2) + 2 * r2)
            | b  && self == nam1 = V2 nxtPx1 nxtPy1
            | b                  = V2 nxtPx2 nxtPy2
            | otherwise          = pos

integralWith' :: (Fractional a, HasTime t s) => (b -> w -> a ->  a) -> a -> Wire s e m (a, b, w) (a, a)
integralWith' correct = loop'
  where
    loop' x' = mkPure $ \ds (vel, n, c) ->
      let dt = realToFrac (dtime ds)
          x  = correct n c (x' + dt*vel)
      in x' `seq` (Right (x', dt), loop' x)


ball :: (HasTime t s) => String -> Double -> Double -> V2 Double -> V2 Double -> Wire s () IO (String, NextS) Ball
ball name' m r v0 p0 = proc (self, nextS) -> do
  vel       <- velocity v0 -< (self, nextS)
  (pos, dt) <- position p0 -< (vel, self, nextS)
  returnA -< makeBall name' vel pos dt
  where makeBall :: String -> V2 Double -> V2 Double -> V2 Double -> Ball
        makeBall n = Ball n m r