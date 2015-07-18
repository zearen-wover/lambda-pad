import Game.LambdaPad ( lambdaPad )
import Game.LambdaPad.Pads.F310 ( f310 )
import Game.LambdaPad.Games.GuildWars2 ( guildWars2 )

import qualified Graphics.XHB as X
import qualified Test.Robot as Robot
import qualified Test.Robot.Connection ( runRobotWith )

data UserData = UserData
  { xConnection :: !X.Connection
  }

main :: IO ()
main = do
  xConn <- X.connect >>= maybe (fail "Failed to connect to X") return
  lambdaPad (UserData xConn) f310 guildWars2
