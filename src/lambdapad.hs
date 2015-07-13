import Game.LambdaPad ( lambdaPad )
import Game.LambdaPad.Pads.F310 ( f310 )

main :: IO ()
main = do
  lambdaPad () f310
