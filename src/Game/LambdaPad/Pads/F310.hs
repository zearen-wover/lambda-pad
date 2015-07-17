module Game.LambdaPad.Pads.F310 ( f310 ) where

import Game.LambdaPad

f310 :: PadConfig user
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
          [ (0, c)
          , (1, n)
          , (3, ne)
          , (2, e)
          , (6, se)
          , (4, s)
          , (12, sw)
          , (8, w)
          , (9, nw)
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
