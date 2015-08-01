module Game.LambdaPad.GameConfig
  ( PackagedGameConfig, package, unpackage, packageName
  , module Game.LambdaPad.Core.GameConfig
  ) where

import Game.LambdaPad.Core ( startLambdaPad, Stop )
import Game.LambdaPad.Core.PadConfig ( PadConfig )
import Game.LambdaPad.Core.GameConfig

data PackagedGameConfig = PackagedGameConfig
    { packageName :: String
    , unpackage :: Float -> PadConfig -> IO Stop
    }

package :: GameConfig user -> PackagedGameConfig
package gameConfig = PackagedGameConfig
    { packageName = gameName gameConfig
    , unpackage = \speed pad -> startLambdaPad speed pad gameConfig
    }
