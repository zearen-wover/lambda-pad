{-# LANGUAGE TemplateHaskell #-}
module Game.LambdaPad.Games.GuildWars2 ( guildWars2 ) where

import Control.Monad ( when )
import Control.Monad.IO.Class( liftIO )
import Control.Monad.State.Class( get )
import Control.Lens ( (^.), view, _1, _2 )
import Control.Lens.TH ( makeLenses )
import Data.Algebra.Boolean (Boolean(..))
import Prelude hiding ( (&&), (||),  not )

import Game.LambdaPad.GameConfig
import Test.Robot ( Robot, Pressable(press, release, hold), moveBy, moveTo )
import Test.Robot.Connection ( runRobotWith )

import qualified Graphics.XHB as X
import qualified Graphics.XHB.Connection as X
import qualified Graphics.XHB.Gen.Shape as X
import qualified Test.Robot.Types as K

data GuildWars2State = GuildWars2State
    { gw2SConn :: !X.Connection
    , gw2MouseSpeed :: !Float
    , _gw2MouseResidual :: !(Float, Float)
    }
makeLenses ''GuildWars2State

guildWars2 :: Float -> GameConfig GuildWars2State
guildWars2 mouseSpeed = GameConfig
    { newUserData = do
          xConn <- maybe (fail "Could not connect to X server") return =<<
              X.connect
          (width, height) <- getScreenSize xConn
          return $ GuildWars2State
              { gw2SConn = xConn
              , gw2MouseSpeed = mouseSpeed * max width height
              , _gw2MouseResidual = (0, 0)
              }
    , onStop = const $ return ()
    , onEvents = do
          buttonAsKey a K.leftButton true
          onTick $ do
            mouseSpeed <- fmap gw2MouseSpeed get
            x' <- withResidual 0.1 mouseSpeed
                (gw2MouseResidual._1) (rightStick.horiz)
            y' <- withResidual 0.1 mouseSpeed
                (gw2MouseResidual._2) (rightStick.vert)
            runRobot $ moveBy x' (negate y')
    }

runRobot :: Robot a -> LambdaPad GuildWars2State a
runRobot rbt = fmap gw2SConn get >>= liftIO . flip runRobotWith rbt

buttonAsKey :: Pressable key
            => PadButton -> key -> Filter GuildWars2State
            -> GameWriter GuildWars2State ()
buttonAsKey but key filter' = do
    onButtonPress but filter' $ runRobot $ press key
    onButtonRelease but filter' $ runRobot $ release key

-- TODO: This should be broken out into its own library.
getScreenSize :: X.Connection -> IO (Float, Float)
getScreenSize xConn = do
    receipt <- X.queryExtents xConn $ X.getRoot xConn
    eiReply <- X.getReply receipt
    case eiReply of
      Left someError -> fail $ show someError
      Right reply -> return
          ( fromIntegral $
                X.bounding_shape_extents_width_QueryExtentsReply reply
          , fromIntegral $
                X.bounding_shape_extents_height_QueryExtentsReply reply
          )
