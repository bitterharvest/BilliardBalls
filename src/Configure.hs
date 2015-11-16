module Configure where

import Linear.V2

-- one unit is equal to 10 meters
data Ball = Ball {name :: String,  mass :: Double, semidiameter :: Double, speed :: V2 Double, location :: V2 Double, step :: V2 Double} deriving (Eq, Show, Read)


radius1 :: Double
radius1 = 0.05

radius2 :: Double
radius2 = 0.05

mass1 :: Double
mass1 = 1.0
mass2 :: Double
mass2 = 1.0

{-
--              ^Y    
--       <- Y   | 
--      ^     <-   
--     \    R R    
pInit1 :: V2 Double
pInit1 = V2 0 0

vInit1 :: V2 Double
vInit1 = V2 (-0.1) 0

pInit2 :: V2 Double
pInit2 = V2 (-0.05) (-(sqrt 3 * 0.05 + 0.001)/2)

vInit2 :: V2 Double
vInit2 = V2 0 0.1
-}

{-
--              ^R    
--       <- R   | 
--      ^     <-   
--     \    Y Y   
pInit2 :: V2 Double
pInit2 = V2 0 0

vInit2 :: V2 Double
vInit2 = V2 (-0.1) 0

pInit1 :: V2 Double
pInit1 = V2 (-0.05) (-(sqrt 3 * 0.05 + 0.001)/2)

vInit1 :: V2 Double
vInit1 = V2 0 0.1
-}

{-
--               Y    
--       <- Y   ->  
--      ^      |   
--      |   R Rv     

pInit2 :: V2 Double
pInit2 = V2 0 0

vInit2 :: V2 Double
vInit2 = V2 0.1 0

pInit1 :: V2 Double
pInit1 = V2 0.05 ((sqrt 3 * 0.05 + 0.001)/2)

vInit1 :: V2 Double
vInit1 = V2 0 (-0.1)
-}

{-
--       | R     R    
--       v      ->  
--     ->      |   
--     Y      Yv     

pInit1 :: V2 Double
pInit1 = V2 0 0

vInit1 :: V2 Double
vInit1 = V2 0.1 0

pInit2 :: V2 Double
pInit2 = V2 0.05 ((sqrt 3 * 0.05 + 0.001)/2)

vInit2 :: V2 Double
vInit2 = V2 0 (-0.1)
-}

{-
--        / Y
--       V      
--     ->     <- \  Y
--     R      R   v  

pInit2 :: V2 Double
pInit2 = V2 0 0

vInit2 :: V2 Double
vInit2 = V2 0.1 0

pInit1 :: V2 Double
pInit1 = V2 0.05 ((sqrt 3 * 0.05 + 0.001)/2)

vInit1 :: V2 Double
vInit1 = V2 (-0.1) (-0.05)
-}

{-
--        / R 
--       V      
--     ->     <- \  R
--     Y      Y   v  


pInit1 :: V2 Double
pInit1 = V2 0 0

vInit1 :: V2 Double
vInit1 = V2 0.1 0

pInit2 :: V2 Double
pInit2 = V2 0.05 ((sqrt 3 * 0.05 + 0.001)/2)

vInit2 :: V2 Double
vInit2 = V2 (-0.1) (-0.05)
-}

{-
--        / Y     Y
--       V      ->
--     ->      /
--     R      v   R

pInit2 :: V2 Double
pInit2 = V2 0 0

vInit2 :: V2 Double
vInit2 = V2 0.1 0

pInit1 :: V2 Double
pInit1 = V2 0.05 ((sqrt 3 * 0.05 + 0.001)/2)

vInit1 :: V2 Double
vInit1 = V2 (-0.1) (-0.1)
-}

{-
--        / R     R
--       V      ->
--     ->      /
--     Y      v   Y

pInit1 :: V2 Double
pInit1 = V2 0 0

vInit1 :: V2 Double
vInit1 = V2 0.1 0

pInit2 :: V2 Double
pInit2 = V2 0.05 ((sqrt 3 * 0.05 + 0.001)/2)

vInit2 :: V2 Double
vInit2 = V2 (-0.1) (-0.1)
-}

{-
--        / Y    
--       V      
--     ->     <- \  Y
--     R      R   v  

pInit2 :: V2 Double
pInit2 = V2 0 0

vInit2 :: V2 Double
vInit2 = V2 0.1 0

pInit1 :: V2 Double
pInit1 = V2 0.05 ((sqrt 3 * 0.05 + 0.001)/2)

vInit1 :: V2 Double
vInit1 = V2 (-0.05) (-0.1)
-}

{-
--        / R   
--       V      
--     ->     <- \  R
--     Y      Y   v  


pInit1 :: V2 Double
pInit1 = V2 0 0

vInit1 :: V2 Double
vInit1 = V2 0.1 0

pInit2 :: V2 Double
pInit2 = V2 0.05 ((sqrt 3 * 0.05 + 0.001)/2)

vInit2 :: V2 Double
vInit2 = V2 (-0.05) (-0.1)
-}

{-
--        Y       ^ Y
--       <-      /
--     ->      /
--     R      v     R

pInit2 :: V2 Double
pInit2 = V2 0 0

vInit2 :: V2 Double
vInit2 = V2 0.1 0

pInit1 :: V2 Double
pInit1 = V2 0.05 ((sqrt 3 * 0.05 + 0.001)/2)

vInit1 :: V2 Double
vInit1 = V2 (-0.1) 0
-}
{-
--        R       ^ R
--       <-      /
--     ->      /
--     Y      v     Y

pInit1 :: V2 Double
pInit1 = V2 0 0

vInit1 :: V2 Double
vInit1 = V2 0.1 0

pInit2 :: V2 Double
pInit2 = V2 0.05 ((sqrt 3 * 0.05 + 0.001)/2)

vInit2 :: V2 Double
vInit2 = V2 (-0.1) 0
-}

{-
pInit1 :: V2 Double
pInit1 = V2 0 0.095

vInit1 :: V2 Double
vInit1 = V2 (-0.6) 0

pInit2 :: V2 Double
pInit2 = V2 (-0.75) 0

vInit2 :: V2 Double
vInit2 = V2 0.2 0
-}

{-
pInit1 :: V2 Double
pInit1 = V2 0.5 0

vInit1 :: V2 Double
vInit1 = V2 (-0.3) 0

pInit2 :: V2 Double
pInit2 = V2 0.75 0

vInit2 :: V2 Double
vInit2 = V2 (-0.4) 0
-}
{-
pInit1 :: V2 Double
pInit1 = V2 (-0.75) 0

vInit1 :: V2 Double
vInit1 = V2 0.2 0

pInit2 :: V2 Double
pInit2 = V2 0.75 0

vInit2 :: V2 Double
vInit2 = V2 (-0.1) 0
-}

pInit1 :: V2 Double
pInit1 = V2 0.5 0.5

vInit1 :: V2 Double
vInit1 = V2 0.2 (-0.3)

pInit2 :: V2 Double
pInit2 = V2 (-0.5) 0.75

vInit2 :: V2 Double
vInit2 = V2 0.1 (-0.4)


data BallC = BallC {ballC :: Bool, name1 :: String, nextV1 :: V2 Double , nextP1 :: V2 Double, name2 :: String, nextV2 :: V2 Double, nextP2 :: V2 Double}  deriving (Eq, Show, Read)
data WallC = WallC {hittingV1 :: Bool, hittingV2 :: Bool, hittingH1 :: Bool, hittingH2 :: Bool}  deriving (Eq, Show, Read)
data NextS = NextS {colliding :: BallC, ball1 :: Ball, ball2 :: Ball, hitting :: WallC} deriving (Eq, Show, Read)


wallV :: Double
wallV = 0.9
wallH :: Double
wallH = 0.9