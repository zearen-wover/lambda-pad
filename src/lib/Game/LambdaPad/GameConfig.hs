module Game.LambdaPad.GameConfig
  ( PackagedGameConfig, package, unpackage, packageName
  , module Game.LambdaPad.Core.GameConfig
  ) where

import Game.LambdaPad.Core.Run
    ( startLambdaPad, Stop, PadConfigSelector )
import Game.LambdaPad.Core.GameConfig

data PackagedGameConfig = PackagedGameConfig
    { packageName :: String
    , unpackage :: PadConfigSelector -> Int -> Float -> IO Stop
    }

package :: GameConfig user -> PackagedGameConfig
package gameConfig = PackagedGameConfig
    { packageName = gameName gameConfig
    , unpackage = \padSelector padIndex speed ->
        startLambdaPad padSelector padIndex gameConfig speed
    }
