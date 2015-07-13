module Game.LambdaPad.Pads.F310 ( f310 ) where

import Control.Monad.Trans ( liftIO )

import Game.LambdaPad

f310 :: PadConfig user
f310 = PadConfig
    { onButton = simpleButtonConfig
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
    , onHat = simpleHatConfig 0
        [ (0, Nothing)
        , (1, Just N)
        , (3, Just NE)
        , (2, Just E)
        , (6, Just SE)
        , (4, Just S)
        , (12, Just SW)
        , (8, Just W)
        , (9, Just NW)
        ]
    , onAxis = \axis raw -> liftIO $ putStrLn $
          "Axis " ++ show axis ++ " is " ++ show raw
    }
