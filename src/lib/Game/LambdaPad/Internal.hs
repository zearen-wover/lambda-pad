module Game.LambdaPad.Internal where

import Game.LambdaPad.Core
    ( Stop, stop )
import Game.LambdaPad.GameConfig
    ( PackagedGameConfig, unpackage, packageName )
import Game.LambdaPad.PadConfig
    ( PadConfig, padName )

data LambdaPadConfig = LambdaPadConfig
    { gameConfigs :: [PackagedGameConfig]
    , padConfigs :: [PadConfig]
    , tickSpeed :: Float
    }

defaultLambdaPadConfig :: LambdaPadConfig
defaultLambdaPadConfig = LambdaPadConfig
    { gameConfigs = []
    , padConfigs = []
    , tickSpeed = 60
    }

lambdaPad :: LambdaPadConfig -> IO ()
lambdaPad _ = return ()
