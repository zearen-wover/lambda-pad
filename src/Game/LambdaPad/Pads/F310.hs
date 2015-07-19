module Game.LambdaPad.Pads.F310 ( f310 ) where

import Game.LambdaPad.PadConfig

f310 :: PadConfig
f310 = PadConfig
    { buttonConfig = simpleButtonConfig
          [ (0, a)
          , (1, b)
          , (2, x)
          , (3, y)
          , (4, lb)
          , (5, rb)
          , (6, back)
          , (7, start)
          , (8, home)
          , (9, ls)
          , (10, rs)
          ]
    , dpadConfig = simpleDPadConfig 0
          [ (0, C)
          , (1, N)
          , (3, NE)
          , (2, E)
          , (6, SE)
          , (4, S)
          , (12, SW)
          , (8, W)
          , (9, NW)
          ]
    , axisConfig = simpleAxisConfig 
          [ (0, horizAxisConfig leftStick)
          , (1, vertAxisConfig leftStick)
          , (2, triggerConfig leftTrigger)
          , (3, horizAxisConfig rightStick)
          , (4, vertAxisConfig rightStick)
          , (5, triggerConfig rightTrigger)
          ]
    }
