{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
module Game.LambdaPad.Internal where

import Control.Concurrent
  ( ThreadId, forkIO, yield )
import Control.Concurrent.MVar
  ( MVar, newMVar, isEmptyMVar, takeMVar, putMVar )
import Control.Monad ( when )
import Control.Monad.State.Strict ( StateT, evalStateT, execStateT, runStateT, get )
import Control.Monad.Trans ( MonadIO, liftIO )
import Data.Int ( Int16 )
import Data.Word ( Word8 )

import Control.Lens ( ALens', Lens', lens, (.=), (^.), use, cloneLens )
import Prelude hiding ( (.), id )
import Control.Category ( (.), id )
import Control.Lens.TH ( makeLenses )

import qualified Data.HashMap as HM
import qualified Data.Vector as V
import qualified SDL

newtype Stop = Stop {stop :: IO ()}

data Button = Button
    { _pressed :: !Bool
    }
  deriving (Show)
makeLenses ''Button

data Dir = N | NE | E | SE | S | SW | W | NW
  deriving (Show)

data DPad = DPad
    { _dir :: !(Maybe Dir)
    }
  deriving (Show)
makeLenses ''DPad

data Trigger = Trigger
  { _pull :: !Float
  }
  deriving (Show)
makeLenses ''Trigger

data Axis = Axis
    { _horiz :: !Float
    , _vert :: !Float
    }
  deriving (Show)
makeLenses ''Axis

tilt :: Lens' Axis (Float, Float)
tilt = lens getter setter
  where getter (Axis horiz' vert') = (horiz', vert')
        setter _ (horiz', vert') = Axis horiz' vert'

data Pad = Pad
    { _a :: !Button
    , _b :: !Button
    , _x :: !Button
    , _y :: !Button
    , _lb :: !Button
    , _rb :: !Button
    , _rs :: !Button
    , _ls :: !Button
    , _start :: !Button
    , _back :: !Button
    , _home :: !Button
    , _dpad :: !DPad
    , _leftTrigger :: !Trigger
    , _rightTrigger :: !Trigger
    , _leftStick :: !Axis
    , _rightStick :: !Axis
    }
  deriving (Show)
makeLenses ''Pad

neutralPad :: Pad
neutralPad = Pad
    { _a = Button False
    , _b = Button False
    , _x = Button False
    , _y = Button False
    , _lb = Button False
    , _rb = Button False
    , _rs = Button False
    , _ls = Button False
    , _start = Button False
    , _back = Button False
    , _home = Button False
    , _dpad = DPad Nothing
    , _leftTrigger = Trigger 0.0
    , _rightTrigger = Trigger 0.0
    , _leftStick = Axis 0.0 0.0
    , _rightStick = Axis 0.0 0.0
    }

type LambdaPad user = StateT (LambdaPadData user) IO

type LambdaPadInner user = StateT (MVar (LambdaPadData user)) IO

data PadConfig user = PadConfig
    { onButton :: Word8 -> Bool -> LambdaPad user ()
    , onHat :: Word8 -> Word8 -> LambdaPad user ()
    , onAxis :: Word8 -> Int16 -> LambdaPad user ()
    }
data LambdaPadData user = LambdaPadData
    { _lpUserData :: !user
    , _lpJoystick :: SDL.Joystick
    , _lpPad :: !Pad
    , _lpOnTick :: LambdaPad user ()
    , _lpInterval :: Float -- ^ In seconds.
    , _lpPadConfig :: !(PadConfig user)
    -- , _lpButtonFilter :: HM.HashMap Button [Filter, LambdaPad user ()]
    -- , _lpAxisFilter :: HM.HashMap Axis [Filter, LambdaPad user ()]
    }
makeLenses ''LambdaPadData

simpleButtonConfig :: 
  [(Word8, ALens' Pad Button)] ->
  Word8 -> Bool -> LambdaPad user ()
simpleButtonConfig rawMapping button isPressed =
    flip (maybe $ return ()) (HM.lookup button mapping) $ \but ->
    (lpPad.cloneLens but.pressed) .= isPressed
  where !mapping = HM.fromList rawMapping 

simpleHatConfig ::
  Word8 -> [(Word8, Maybe Dir)] -> Word8 -> Word8 -> LambdaPad user ()
simpleHatConfig hatIndex rawMapping hat dirWord = when (hatIndex == hat) $
    flip (maybe $ return ()) (HM.lookup dirWord mapping)
    ((lpPad.dpad.dir).=)
  where !mapping = HM.fromList rawMapping 

simpleAxisConfig ::
  [(Word8, Int16 -> LambdaPad user ())] -> Word8 -> Int16 -> LambdaPad user ()
simpleAxisConfig rawMapping axis =
    maybe (const $ return ()) id $ HM.lookup axis mapping
  where !mapping = HM.fromList rawMapping 

axisConfig :: (Float -> Float) -> ALens' Pad Float -> Int16 -> LambdaPad user ()
axisConfig orient valueLens rawVal = (lpPad.cloneLens valueLens) .= val
  where val = orient $ fromIntegral rawVal / fromIntegral (minBound :: Int16)

triggerConfig :: ALens' Pad Trigger -> Int16 -> LambdaPad user ()
triggerConfig trig rawVal = (lpPad.cloneLens trig.pull) .= val
  where val = (fromIntegral rawVal - fromIntegral (minBound :: Int16)) /
              (fromIntegral (maxBound :: Int16) -
               fromIntegral (minBound :: Int16))

withLambdaPad :: LambdaPad user a -> LambdaPadInner user a
withLambdaPad act = do
    lambdaPadData <- liftIO . takeMVar =<< get
    (val, lambdaPadData') <- liftIO $ runStateT act lambdaPadData
    liftIO . flip putMVar lambdaPadData' =<< get
    return val

lambdaPad :: user -> PadConfig user -> IO ()
lambdaPad userData padConfig = do
    SDL.initialize [SDL.InitJoystick]
    numSticks <- SDL.numJoysticks
    joysticks <- SDL.availableJoysticks
    when (numSticks > 0) $ do
      joystick <- SDL.openJoystick $ V.head joysticks
      putStrLn "Starting to listen."
      mvarLambdaPadData <- newMVar $ LambdaPadData
          { _lpUserData = userData
          , _lpJoystick = joystick
          , _lpPadConfig = padConfig
          , _lpPad = neutralPad
          , _lpOnTick = liftIO . print =<< use lpPad
          , _lpInterval = 1 / 60
          }
      eventLoop <- initEventLoop mvarLambdaPadData
      tickLoop <- initTickLoop mvarLambdaPadData
      _ <- getLine
      stop tickLoop
      stop eventLoop
    SDL.quit

type LoopIn m = m () -> m ()

runLoopIn :: MonadIO io => (LoopIn io -> IO ()) -> IO (Stop, ThreadId)
runLoopIn acquire = do
    mvarStop <- newMVar ()
    tid <- forkIO $ acquire $ loopIn mvarStop
    return (Stop $ stopLoop mvarStop, tid)
  where loopIn :: MonadIO io => MVar () -> LoopIn io
        loopIn mvarStop act = do
              act
              liftIO $ yield
              stop <- liftIO $ isEmptyMVar mvarStop
              if stop
                then liftIO $ putMVar mvarStop ()
                else loopIn mvarStop act

stopLoop :: MVar () -> IO ()
stopLoop mvarStop = do
    takeMVar mvarStop -- Signal readLoop
    takeMVar mvarStop -- Wait for readLoop

initEventLoop :: MVar (LambdaPadData user) -> IO Stop
initEventLoop mvarLambdaPadData = do
    (eventLoop, _) <- runLoopIn $ \loopIn ->
        flip evalStateT mvarLambdaPadData $ loopIn listenEvent
    return eventLoop

listenEvent :: LambdaPadInner user ()
listenEvent = SDL.waitEventTimeout 1000 >>=
    maybe (return ()) (withLambdaPad . listen')
  where listen' :: SDL.Event -> LambdaPad user ()
        listen' SDL.Event{SDL.eventPayload} = do
            case eventPayload of
              SDL.JoyButtonEvent (SDL.JoyButtonEventData
                {SDL.joyButtonEventButton, SDL.joyButtonEventState}) -> do
                  on <- fmap onButton $ use lpPadConfig
                  case joyButtonEventState of
                    0 -> on joyButtonEventButton False
                    1 -> on joyButtonEventButton True
                    _ -> liftIO $ putStrLn $
                      "Unrecognized button state: " ++ show joyButtonEventState
              SDL.JoyHatEvent (SDL.JoyHatEventData
                {SDL.joyHatEventHat, SDL.joyHatEventValue}) -> do
                  on <- fmap onHat $ use lpPadConfig
                  on joyHatEventHat joyHatEventValue
              SDL.JoyAxisEvent (SDL.JoyAxisEventData
                {SDL.joyAxisEventAxis, SDL.joyAxisEventValue}) -> do
                  on <- fmap onAxis $ use lpPadConfig
                  on joyAxisEventAxis joyAxisEventValue
              _ -> return ()

initTickLoop :: MVar (LambdaPadData user) -> IO Stop
initTickLoop mvarLambdaPadData = do
    mvarStop <- newMVar ()
    _ <- SDL.addTimer 0 $ listenTick mvarStop mvarLambdaPadData
    return $ Stop $ stopLoop mvarStop

listenTick :: MVar () -> MVar (LambdaPadData user) -> SDL.TimerCallback
listenTick mvarStop mvarLambdaPadData _ = do
    startTime <- SDL.ticks
    print startTime
    lambdaPadData <- takeMVar mvarLambdaPadData
    lambdaPadData' <- execStateT (lambdaPadData^.lpOnTick) lambdaPadData
    let interval = lambdaPadData'^.lpInterval
    putMVar mvarLambdaPadData lambdaPadData'

    stop <- isEmptyMVar mvarStop
    if stop
      then do
        liftIO $ putMVar mvarStop ()
        return SDL.Cancel
      else do
        endTime <- liftIO $ SDL.ticks
        return $ SDL.Reschedule $
            (floor (interval * 1000) - (endTime - startTime))
