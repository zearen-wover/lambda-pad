import Game.LambdaPad ( lambdaPad )
import Game.LambdaPad.Pads.F310 ( f310 )
import Game.LambdaPad.Games.GuildWars2 ( {- guildWars2 -} )

import qualified Graphics.XHB as X

main :: IO ()
main = do
  lambdaPad () f310
