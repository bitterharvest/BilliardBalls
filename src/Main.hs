{-# LANGUAGE Arrows #-}
-- module Main where

import Prelude hiding ((.),)
import Control.Wire
import Control.Monad
import Control.Monad.IO.Class()
import FRP.Netwire()
import Graphics.Rendering.OpenGL 
import Graphics.UI.GLFW 
import Data.IORef
import Linear.V2
import Control.Monad.IO.Class

import Ball
import Configure
import Collision

type Point = (Double, Double)
type Polygon = [Point]

renderPoint :: Point -> IO () 
renderPoint (x, y) = vertex $ Vertex2 (realToFrac x :: GLfloat) (realToFrac y :: GLfloat)

generatePointsForBall :: Ball -> Polygon 
generatePointsForBall (Ball _ _ r (V2 _ _) (V2 x y) (V2 _ _)) = 
  map (\t -> (x+r*cos t, y+r*sin t)) [0,0.2..2*pi]

generatePointsForLeftWall :: Polygon 
generatePointsForLeftWall = 
  [ (-1, -1) 
  , (- wallV, -1) 
  , (- wallV, 1) 
  , (-1, 1) ] 

generatePointsForRightWall :: Polygon 
generatePointsForRightWall = 
  [ (1, 1) 
  , (wallV, 1) 
  , (wallV, -1) 
  , (1, -1) ] 
generatePointsForUpWall :: Polygon 
generatePointsForUpWall = 
  [ (1, 1) 
  , (-1, 1) 
  , (-1, wallH) 
  , (1, wallH) ] 

generatePointsForDownWall :: Polygon 
generatePointsForDownWall = 
  [ (-1, -1) 
  , (1, -1) 
  , (1, -wallH) 
  , (-1, -wallH) ] 

runNetwork :: (HasTime t s) => IORef Bool -> Session IO s -> Wire s e IO a (Ball, Ball) -> IO () 
runNetwork closedRef session wire = do 
  pollEvents 
  let color3f r g b = color $ Color3 r g (b :: GLfloat)
  closed <- readIORef closedRef 
  Control.Monad.unless closed $ do
      (st , session') <- stepSession session 
      (wt', wire' ) <- stepWire wire st $ Right undefined 
      case wt' of 
        Left _ -> return () 
        Right (b1,b2) -> do
          clear [ColorBuffer] 
          color3f 1.0 0.8 0.6
          renderPrimitive Polygon $ 
            mapM_ renderPoint $ generatePointsForBall b1
          color3f 0.8 0.2 0.2
          renderPrimitive Polygon $ 
            mapM_ renderPoint $ generatePointsForBall b2

          color3f 0.7 0.7 0.7
          renderPrimitive Polygon $ 
            mapM_ renderPoint generatePointsForLeftWall
          renderPrimitive Polygon $ 
            mapM_ renderPoint generatePointsForRightWall
          renderPrimitive Polygon $ 
            mapM_ renderPoint generatePointsForUpWall
          renderPrimitive Polygon $ 
            mapM_ renderPoint generatePointsForDownWall
          swapBuffers 
          runNetwork closedRef session' wire' 

simulation :: HasTime t s => Wire s () IO a (Ball, Ball)
simulation = proc _ -> do
    rec b1 <- ball "ball1" mass1 radius1 vInit1 pInit1 -< ("ball1", c)
        b2 <- ball "ball2" mass2 radius2 vInit2 pInit2 -< ("ball2", c)
        c <- collision -< (b1, b2) 
    returnA -< (b1, b2)

main1 :: IO ()
--main1 = testWireM liftIO (countSession_ 0.01) simulation
main1 = testWireM liftIO clockSession_ simulation

main :: IO () 
main = do
  _ <- initialize 
  _ <- openWindow (Size 640 640) [DisplayRGBBits 8 8 8, DisplayAlphaBits 8, DisplayDepthBits 24] Window
  closedRef <- newIORef False 
  windowCloseCallback $= do 
    writeIORef closedRef True 
    return True 
--  runNetwork closedRef (countSession_ 0.01) simulation
  runNetwork closedRef clockSession_ simulation
  closeWindow
