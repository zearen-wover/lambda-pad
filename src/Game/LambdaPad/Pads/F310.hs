module Game.LambdaPad.Pads.F310 ( f310 ) where

import Control.Monad.Trans ( liftIO )

import Game.LambdaPad

f310 = PadConfig
    { onButton = \but isPressed -> liftIO $ putStrLn $
          "Button " ++ show but ++ if isPressed then "pressed" else "released"
    , onHat = \hat dir' -> liftIO $ putStrLn $
          "Hat " ++ show hat ++ " is " ++ show dir'
    , onAxis = \axis raw -> liftIO $ putStrLn $
          "Axis " ++ show axis ++ " is " ++ show raw
    }
