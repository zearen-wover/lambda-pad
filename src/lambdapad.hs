import Game.LambdaPad ( lambdaPad )
import Game.LambdaPad.Pads.F310 ( f310 )
import Game.LambdaPad.Games.GuildWars2 ( {- guildWars2 -} )

main :: IO ()
main = do
  lambdaPad () f310
