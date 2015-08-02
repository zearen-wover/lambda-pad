module Main where

import Game.LambdaPad

main :: IO ()
main = lambdaPad defaultLambdaPadConfig

{-
import Game.LambdaPad
import Game.LambdaPad.GameConfig

import Control.Monad.IO.Class ( liftIO )
import Data.Algebra.Boolean ( true )

foo :: PackagedGameConfig
foo = package $ GameConfig
    { gameName = "foo"
    , newUserData = return ()
    , onStop = const $ return ()
    , onEvents = do
        onButtonPress a true $ liftIO $ putStrLn "A foo"
        onTick $ liftIO $ putStrLn "Tick"
    }

bar :: PackagedGameConfig
bar = package $ GameConfig
    { gameName = "bar"
    , newUserData = return ()
    , onStop = const $ return ()
    , onEvents = do
        onButtonPress a true $ liftIO $ putStrLn "B foo"
    }

main :: IO ()
main = lambdaPad defaultLambdaPadConfig
  { gameConfigs = [ foo, bar ]
  , defaultGame = Just foo
  }
-}
