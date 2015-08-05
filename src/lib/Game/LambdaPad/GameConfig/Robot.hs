{-# LANGUAGE FlexibleContexts #-}
module Game.LambdaPad.GameConfig.Robot
    ( HasRobot(getConn)
    , Connection, connect
    , getScreenSize
    , runRobot
    , pressKeys, releaseKeys
    , buttonAsKeys
    , dirAsKeys
    , stickAsKeys
    , triggerAsKeys
    , stickAsMouse
    , stickAsScroll
    )  where

import Control.Monad ( replicateM_ )
import Control.Monad.IO.Class( liftIO )
import Control.Monad.State.Class( get )
import Control.Lens ( ALens', cloneLens, _1, _2 )
import Test.Robot ( Robot, Pressable(press, release), moveBy, tap )
import Test.Robot.Connection ( runRobotWith )

import qualified Graphics.XHB as X
import qualified Graphics.XHB.Gen.Shape as X
import qualified Test.Robot.Types as K

import Game.LambdaPad.GameConfig

newtype Connection = Connection { rawConnection :: X.Connection }

class HasRobot user where
    getConn :: user -> Connection

connect :: IO Connection
connect = maybe (fail "Could not connect to X server") (return . Connection)
    =<< X.connect

getScreenSize :: Connection -> IO (Int, Int)
getScreenSize (Connection xConn) = do
    -- TODO: Support multiple displays.
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

runRobot :: HasRobot user => Robot a -> LambdaPad user a
runRobot rbt = fmap getConn get >>=
    liftIO . flip runRobotWith rbt . rawConnection

pressKeys :: (Pressable key, HasRobot user) => [key] -> LambdaPad user ()
pressKeys keys = mapM_ (runRobot . press) keys

releaseKeys :: (Pressable key, HasRobot user) => [key] -> LambdaPad user ()
releaseKeys keys = mapM_ (runRobot . release) $ reverse keys

buttonAsKeys :: (Pressable key, HasRobot user)
             => PadButton -> Filter user -> [key] -> GameWriter user ()
buttonAsKeys but filter' keys = do
    onButtonPress but filter' $ pressKeys keys
    onButtonRelease but filter' $ releaseKeys keys

dirAsKeys :: (Pressable key, HasRobot user)
          => Direction -> Filter user -> [key] -> DPadButton user
dirAsKeys dir' filter' keys = dPadButton
    dir' filter' (pressKeys keys) (releaseKeys keys)

stickAsKeys :: (Pressable key, HasRobot user, WithFilter Stick stickFilter)
            => ALens' user Bool -> PadStick -> stickFilter -> Filter user
            -> [key] -> GameWriter user ()
stickAsKeys isTilted stick stickFilter filter' keys =
    withStick isTilted stick stickFilter filter'
        ( pressKeys keys
        , releaseKeys keys
        )

triggerAsKeys :: (Pressable key, HasRobot user,
                  WithFilter Trigger triggerFilter)
              => ALens' user Bool -> PadTrigger -> triggerFilter -> Filter user
              -> [key] -> GameWriter user ()
triggerAsKeys isPulled trigger triggerFilter filter' keys =
    withTrigger isPulled trigger triggerFilter filter'
        ( pressKeys keys
        , releaseKeys keys
        )

stickResiduals :: HasRobot user
               => Float -> Float -> ALens' user (Float, Float) -> PadStick
               -> LambdaPad user (Int, Int)
stickResiduals deadZone speed residuals stick = do
    x' <- withResidual deadZone speed (cloneLens residuals._1)
        (cloneLens stick.horiz)
    y' <- withResidual deadZone speed (cloneLens residuals._2)
        (cloneLens stick.vert)
    return (x', y')

stickAsMouse :: HasRobot user
             => Float -- ^ dead zone.
             -> Float -- ^ speed in pixels per second.
             -> ALens' user (Float, Float) -- ^ (x, y) residuals.
             -> PadStick
             -> LambdaPad user ()
stickAsMouse deadZone speed residuals stick = do
    (x', y') <- stickResiduals deadZone speed residuals stick
    runRobot $ moveBy x' $ negate y'

stickAsScroll :: HasRobot user
              => Float -- ^ dead zone.
              -> Float -- ^ speed in scroll clicks per second.
              -> ALens' user (Float, Float) -- ^ (x, y) residuals.
              -> PadStick
              -> LambdaPad user ()
stickAsScroll deadZone speed residuals stick = do
    (x', y') <- stickResiduals deadZone speed residuals stick
    scrollFor x' K.scrollLeft K.scrollRight
    scrollFor y' K.scrollDown K.scrollUp
  where scrollFor scroll negKey posKey = runRobot $
            if scroll >= 0
              then replicateM_ scroll $ tap posKey
              else replicateM_ (negate scroll) $ tap negKey
