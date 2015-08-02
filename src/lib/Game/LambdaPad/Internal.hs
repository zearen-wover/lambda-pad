{-# LANGUAGE RecordWildCards #-}
module Game.LambdaPad.Internal where

import Control.Applicative ( (<$>), (<*>), optional )
import Data.Maybe ( fromMaybe, listToMaybe )
import Data.Monoid ( mempty, mconcat, (<>) )
import System.IO ( hPutStrLn, stderr )

import qualified Config.Dyre as Dyre
import qualified Options.Applicative as Opt

import Game.LambdaPad.Core.Run
    ( stop, padConfigByName, padConfigByDefault )
import Game.LambdaPad.GameConfig
    ( PackagedGameConfig, unpackage, packageName )
import Game.LambdaPad.PadConfig
    ( PadConfig, padShortName )
import Game.LambdaPad.Pads ( allKnownPads )

data LambdaPadConfig = LambdaPadConfig
    { gameConfigs :: [PackagedGameConfig]
    , padConfigs :: [PadConfig]
    , defaultGame :: Maybe PackagedGameConfig
    , defaultPad :: Maybe PadConfig
    , defaultSpeed :: Float
    , errorMsg :: Maybe String
    }

defaultLambdaPadConfig :: LambdaPadConfig
defaultLambdaPadConfig = LambdaPadConfig
    { gameConfigs = []
    , padConfigs = allKnownPads
    , defaultGame = Nothing
    , defaultPad = Nothing
    , defaultSpeed = 60
    , errorMsg = Nothing
    }

data LambdaPadFlags = LambdaPadFlags
    { lpfPad :: Maybe PadConfig
    , lpfDefaultPad :: Maybe PadConfig
    , lpfGame :: PackagedGameConfig
    , lpfSpeed :: Float
    , lpfJoyIndex :: Int
    }

lambdaPadFlags :: LambdaPadConfig -> Opt.Parser LambdaPadFlags
lambdaPadFlags (LambdaPadConfig{..}) = LambdaPadFlags
    <$> (optional $ Opt.option padChooser $ mconcat
        [ Opt.long "pad"
        , Opt.short 'p'
        ])
    <*> case defaultPad of
          Nothing -> optional $ Opt.option padChooser $ mconcat
              [ Opt.long "default-pad"
              ]
          Just actualDefaultPad -> fmap Just $ Opt.option padChooser $ mconcat
              [ Opt.long "default-pad"
              , Opt.value actualDefaultPad
              ]
    <*> (Opt.option gameChooser $ mconcat
        [ Opt.long "game"
        , Opt.short 'g'
        , maybe mempty Opt.value defaultGame
        ])
    <*> (Opt.option Opt.auto $ mconcat
        [ Opt.long "speed"
        , Opt.short 's'
        , Opt.value defaultSpeed
        ])
    <*> (Opt.option Opt.auto $ mconcat
        [ Opt.long "joystick"
        , Opt.short 'j'
        , Opt.value 0
        ])
  where chooser :: (String -> String) -> (a -> String) -> [a] -> Opt.ReadM a
        chooser formatError getName as = Opt.eitherReader $ \name ->
            maybe (Left $ formatError name) Right $ listToMaybe $
                filter ((==name) . getName) as
        padChooser = chooser (("Unrecognized pad " ++) . show)
            padShortName padConfigs
        gameChooser = chooser (("Unrecognized game " ++) . show)
            packageName gameConfigs

realLambdaPad :: LambdaPadConfig -> IO ()
realLambdaPad lambdaPadConfig = do
    maybe (return ()) (hPutStrLn stderr) $ errorMsg lambdaPadConfig
    (LambdaPadFlags{..}) <- Opt.execParser $ 
        Opt.info (Opt.helper <*> lambdaPadFlags lambdaPadConfig) $ mconcat
            [ Opt.fullDesc
            , Opt.header "lambda-pad - Control KBM games with your gamepad !"
            ]
    let padConfigSelector = flip fromMaybe ( padConfigByDefault <$> lpfPad) $
          (padConfigByName (padConfigs lambdaPadConfig) <>) $
              maybe mempty padConfigByDefault lpfDefaultPad
    lpStop <- unpackage lpfGame padConfigSelector lpfJoyIndex lpfSpeed
    _ <- getLine
    stop lpStop

showError :: LambdaPadConfig -> String -> LambdaPadConfig
showError cfg msg = cfg { errorMsg = Just msg }

lambdaPad :: LambdaPadConfig -> IO ()
lambdaPad = Dyre.wrapMain $ Dyre.defaultParams
    { Dyre.projectName = "lambda-pad"
    , Dyre.realMain = realLambdaPad
    , Dyre.showError = showError
    }
