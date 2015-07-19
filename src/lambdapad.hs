import Game.LambdaPad ( lambdaPad, stop )
import Game.LambdaPad.Pads.F310 ( f310 )
import Game.LambdaPad.Games.GuildWars2 ( guildWars2 )

main :: IO ()
main = do
  lpLoop <- lambdaPad f310 guildWars2
  _ <- getLine
  stop lpLoop
