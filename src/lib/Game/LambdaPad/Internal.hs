{-# LANGUAGE RecordWildCards #-}
module Game.LambdaPad.Internal where

import Control.Applicative ( (<$>), (<*>), pure, optional )
import Control.Monad ( when )
import Data.Maybe ( fromMaybe, listToMaybe )
import Data.Monoid ( mempty, mconcat, (<>) )
import System.Directory ( doesFileExist, createDirectoryIfMissing )
import System.FilePath ( dropFileName )
import System.IO ( IOMode(WriteMode), withFile, hPutStrLn, )

import qualified Config.Dyre as Dyre
import qualified Config.Dyre.Options as Dyre
import qualified Config.Dyre.Paths as Dyre
import qualified Options.Applicative as Opt

import Game.LambdaPad.Core.Run
    ( Stop, stop, startLambdaPad
    , PadConfigSelector, padConfigByName, padConfigByDefault
    )
import Game.LambdaPad.Core.GameConfig ( GameConfig )
import Game.LambdaPad.PadConfig ( PadConfig, padShortName )
import Game.LambdaPad.Pads ( allKnownPads )

data LambdaPadConfig = LambdaPadConfig
    { gameConfigs :: [PackagedGameConfig]
    , padConfigs :: [PadConfig]
    , defaultPad :: Maybe PadConfig
    , defaultSpeed :: Float
    , errorMsg :: Maybe String
    }

defaultLambdaPadConfig :: LambdaPadConfig
defaultLambdaPadConfig = LambdaPadConfig
    { gameConfigs = []
    , padConfigs = allKnownPads
    , defaultPad = Nothing
    , defaultSpeed = 60
    , errorMsg = Nothing
    }

type LambdaPadGame = PadConfigSelector -> Int -> Float -> IO Stop

data LambdaPadFlags = LambdaPadFlags
    { lpfPad :: Maybe PadConfig
    , lpfSpeed :: Float
    , lpfJoyIndex :: Int
    , lpfLambdaPad :: LambdaPadGame
    }

lambdaPadFlags :: LambdaPadConfig -> Opt.Parser LambdaPadFlags
lambdaPadFlags (LambdaPadConfig{..}) =
    (Opt.infoOption "Version: 0.1.0.0" $ mconcat
     [ Opt.long "version"
     , Opt.help "Print the version and exit."
     ])
    <*> lambdaPadFlags'
  where lambdaPadFlags' = LambdaPadFlags
            <$> (optional $ Opt.option padChooser $ mconcat
                 [ Opt.long "pad"
                 , Opt.short 'p'
                 , Opt.metavar "NAME"
                 , Opt.help "The short name of a pad to use."
                 ])
            <*> (Opt.option Opt.auto $ mconcat
                 [ Opt.long "speed"
                 , Opt.short 's'
                 , Opt.value defaultSpeed
                 , Opt.metavar "FLOAT"
                 , Opt.showDefault
                 , Opt.help $ "The desired speed of the background tick " ++
                     "thread in ticks per second"
                 ])
            <*> (Opt.option Opt.auto $ mconcat
                 [ Opt.long "joystick"
                 , Opt.short 'j'
                 , Opt.value 0
                 , Opt.metavar "INT"
                 , Opt.help "This uses joystick i, where i is a 0 based index."
                 ])
            <*> (Opt.subparser $ mconcat $ map unpackage gameConfigs)
        chooser :: (String -> String) -> (a -> String) -> [a] -> Opt.ReadM a
        chooser formatError getName as = Opt.eitherReader $ \name ->
            maybe (Left $ formatError name) Right $ listToMaybe $
                filter ((==name) . getName) as
        padChooser = chooser (("Unrecognized pad " ++) . show)
            padShortName padConfigs

data PackageConfig user flags = PackageConfig
    { packageCommand :: String
    , packageName :: String
    , packageFlags :: Opt.Parser flags
    , packageGameConfig :: flags -> GameConfig user
    }

newtype PackagedGameConfig = PackagedGameConfig
    { unpackage :: Opt.Mod Opt.CommandFields LambdaPadGame }

package :: PackageConfig user flags -> PackagedGameConfig
package (PackageConfig{..}) = PackagedGameConfig $ Opt.command packageCommand $
    Opt.info (lambdaPadGame <$> packageFlags) $ Opt.header packageName
  where lambdaPadGame flags padSelector padIndex speed =
            startLambdaPad padSelector padIndex (packageGameConfig flags) speed

simplePackage
    :: String -- ^ Package command.
    -> String -- ^ Pacakge name.
    -> GameConfig user
    -> PackagedGameConfig
simplePackage command name gameConfig = package $ PackageConfig
    { packageCommand = command
    , packageName = name
    , packageFlags = pure ()
    , packageGameConfig = const gameConfig
    }

realLambdaPad :: LambdaPadConfig -> IO ()
realLambdaPad lambdaPadConfig = do
    maybe (return ()) fail $ errorMsg lambdaPadConfig
    (LambdaPadFlags{..}) <- Opt.execParser $ 
        Opt.info (Opt.helper <*> lambdaPadFlags lambdaPadConfig) $ mconcat
            [ Opt.fullDesc
            , Opt.header "lambda-pad - Control things with your gamepad !"
            ]
    let padConfigSelector = flip fromMaybe (padConfigByDefault <$> lpfPad) $
          (padConfigByName (padConfigs lambdaPadConfig) <>) $
              maybe mempty padConfigByDefault $ defaultPad lambdaPadConfig
    lpStop <- lpfLambdaPad padConfigSelector lpfJoyIndex lpfSpeed
    _ <- getLine
    stop lpStop

showError :: LambdaPadConfig -> String -> LambdaPadConfig
showError cfg msg = cfg { errorMsg = Just msg }

lambdaPad :: LambdaPadConfig -> IO ()
lambdaPad lambdaPadConfig = do
  (_, _, configFilePath, _, _) <- Dyre.getPaths dyreParams
  let configDirPath = dropFileName configFilePath
  configExists <- doesFileExist configFilePath
  when (not configExists) $ do
      putStrLn $ "Config missing, writing empty config to " ++
          show configFilePath
      createDirectoryIfMissing True $ configDirPath
      withFile configFilePath WriteMode $
          flip mapM_ defaultConfigFile . hPutStrLn
  flip Dyre.wrapMain lambdaPadConfig $ dyreParams
      { Dyre.ghcOpts =
            [ "-threaded", "-funbox-strict-fields", "-i" ++ configDirPath ]
      }
  where dyreParams = Dyre.defaultParams
            { Dyre.projectName = "lambda-pad"
            , Dyre.realMain = Dyre.withDyreOptions dyreParams . realLambdaPad
            , Dyre.showError = showError
            , Dyre.includeCurrentDirectory = False
            }

defaultConfigFile :: [String]
defaultConfigFile = 
    [ "import Game.LambdaPad"
    , ""
    , "main :: IO ()"
    , "main = lambdaPad defaultLambdaPadConfig"
    , "    { gameConfigs = [ ]"
    , "    }"
    ]
