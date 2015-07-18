{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
module Game.LambdaPad.Internal where

import Control.Concurrent
  ( ThreadId, forkIO, yield )
import Control.Concurrent.MVar
  ( MVar, newMVar, isEmptyMVar, takeMVar, putMVar )
import Control.Monad ( when, liftM )
import Control.Monad.State.Strict
    ( StateT, evalStateT, execStateT, runStateT, get )
import Control.Monad.Trans ( MonadIO, liftIO )
import Data.Int ( Int16 )
import Data.Word ( Word8 )

import Data.Algebra.Boolean ( Boolean(..) )
import Control.Lens
    ( ALens', Lens', lens, (.=), (%=), (^.), to, use, cloneLens )
import Control.Category ( (.), id )
import Prelude hiding ( (.), (&&), (||), not, id)
import Control.Lens.TH ( makeLenses )

import qualified Data.HashMap as HM
import qualified Data.Vector as V
import qualified SDL

-- Utilities

whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJust = flip $ maybe $ return ()

(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) = (.).(.)
infixr 1 .:

-- Types, instances and lenses

newtype Stop = Stop {stop :: IO ()}

data Button = Button
    { _butPressed :: !Bool
    , buttonHash :: !Int
    }
makeLenses ''Button

instance Show Button where 
  show but = if but^.butPressed then "but(*)" else "but( )"

data Direction = C | N | NE | E | SE | S | SW | W | NW
  deriving (Show)

data Dir = Dir
    { direction :: !Direction
    , dirHash :: !Int
    }
  deriving (Show)
makeLenses ''Dir

data DPad = DPad
    { _dir :: ALens' DPad Dir
    , _c :: !Dir
    , _n :: !Dir
    , _ne :: !Dir
    , _e :: !Dir
    , _se :: !Dir
    , _s :: !Dir
    , _sw :: !Dir
    , _w :: !Dir
    , _nw :: !Dir
    }
makeLenses ''DPad

instance Show DPad where
    showsPrec _ dpad' =
        ("dpad["++) . (dpad'^.cloneLens (dpad'^.dir).to direction.to shows) .
        ("]"++)

data Trigger = Trigger
  { _pull :: !Float
  , triggerHash :: !Int
  }
makeLenses ''Trigger

instance Show Trigger where
  showsPrec _ trig = ("trig("++) . (trig^.pull.to shows) . (')':)

data Axis = Axis
    { _horiz :: !Float
    , _vert :: !Float
    , axisHash :: !Int
    }
makeLenses ''Axis

tilt :: Lens' Axis (Float, Float)
tilt = lens getter setter
  where getter (Axis horiz' vert' _) = (horiz', vert')
        setter (Axis _ _ h) (horiz', vert') = Axis horiz' vert' h

instance Show Axis where
  showsPrec _ axis = ("trig("++) . (axis^.tilt.to shows) . (')':)

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

newtype Filter = Filter { runFilter :: Pad -> Bool }

makeFilterOp :: (Bool -> Bool -> Bool) -> Filter -> Filter -> Filter
makeFilterOp op a b = Filter $ \tf -> runFilter a tf `op` runFilter a tf

instance Boolean Filter where
  true = Filter $ const True
  false = Filter $ const False
  not = Filter . (not.) . runFilter
  (&&) = makeFilterOp (&&)
  (||) = makeFilterOp (||)
  xor = makeFilterOp (/=)
  (<-->) = makeFilterOp (==)

type LambdaPad user = StateT (LambdaPadData user) IO

type LambdaPadInner user = StateT (MVar (LambdaPadData user)) IO

data PadConfig user = PadConfig
    { buttonConfig :: Word8 -> Bool -> LambdaPad user (Maybe Button)
    , dpadConfig :: Word8 -> Word8 -> LambdaPad user (Maybe Dir)
    , axisConfig :: Word8 -> Int16
                 -> LambdaPad user (Maybe (Either Axis Trigger))
    }

data LambdaPadData user = LambdaPadData
    { _lpUserData :: !user
    , _lpJoystick :: SDL.Joystick
    , _lpPad :: !Pad
    , _lpOnTick :: LambdaPad user ()
    , _lpInterval :: Float -- ^ In seconds.
    , _lpPadConfig :: !(PadConfig user)
    , _lpEventFilter :: HM.HashMap Int [(Filter, LambdaPad user ())]
    }
makeLenses ''LambdaPadData

emptyDPad :: DPad
emptyDPad = DPad
    { _dir = c
    , _c = Dir C 11
    , _n = Dir N 12
    , _ne = Dir NE 13
    , _e = Dir E 14
    , _se = Dir SE 15
    , _s = Dir S 16
    , _sw = Dir SW 17
    , _w = Dir W 18
    , _nw = Dir NW 19
    }

emptyPad :: Pad
emptyPad = Pad
    { _a = Button False 0
    , _b = Button False 1
    , _x = Button False 2
    , _y = Button False 3
    , _lb = Button False 4
    , _rb = Button False 5
    , _rs = Button False 6
    , _ls = Button False 7
    , _start = Button False 8
    , _back = Button False 9
    , _home = Button False 10
    , _dpad = emptyDPad
    , _leftTrigger = Trigger 0.0 20
    , _rightTrigger = Trigger 0.0 21
    , _leftStick = Axis 0.0 0.0 22
    , _rightStick = Axis 0.0 0.0 23
    }

simpleButtonConfig
    :: [(Word8, ALens' Pad Button)]
    -> Word8 -> Bool -> LambdaPad user (Maybe Button)
simpleButtonConfig rawMapping button isPressed = do
    case HM.lookup button mapping of
      Nothing -> return Nothing
      Just but -> do
        (lpPad.cloneLens but.butPressed) .= isPressed
        fmap Just $ use $ lpPad.cloneLens but
  where !mapping = HM.fromList rawMapping 

simpleDPadConfig ::
  Word8 -> [(Word8, ALens' DPad Dir)] -> Word8 -> Word8 -> LambdaPad user (Maybe Dir)
simpleDPadConfig hatIndex rawMapping hat dirWord =
    if hatIndex == hat
      then do
        case HM.lookup dirWord mapping of
          Nothing -> return Nothing
          Just dir' -> do
            (lpPad.dpad.dir) .= dir'
            fmap Just $ use $ lpPad.dpad.cloneLens dir'
      else return Nothing
  where !mapping = HM.fromList rawMapping 

simpleAxisConfig
    :: [(Word8, Int16 -> LambdaPad user (Either Axis Trigger))]
    -> Word8 -> Int16 -> LambdaPad user (Maybe (Either Axis Trigger))
simpleAxisConfig rawMapping axis val =
    case HM.lookup axis mapping of
      Nothing -> return Nothing
      Just axisAct -> fmap Just $ axisAct val
  where !mapping = HM.fromList rawMapping 

horizAxisConfig :: ALens' Pad Axis -> Int16
                -> LambdaPad user (Either Axis Trigger)
horizAxisConfig axis rawVal = do
    (lpPad.cloneLens axis.horiz) .= val
    fmap Left $ use $  lpPad.cloneLens axis
  where val = fromIntegral (if rawVal == minBound
                            then minBound + 1 else rawVal) /
              fromIntegral (maxBound :: Int16)

vertAxisConfig :: ALens' Pad Axis -> Int16
               -> LambdaPad user (Either Axis Trigger)
vertAxisConfig axis rawVal = do
    (lpPad.cloneLens axis.vert) .= val
    fmap Left $ use $  lpPad.cloneLens axis
  where val = negate $ fromIntegral
              (1+if rawVal == maxBound then maxBound - 1 else rawVal) /
              fromIntegral (maxBound :: Int16)

triggerConfig :: ALens' Pad Trigger -> Int16
              -> LambdaPad user (Either Axis Trigger)
triggerConfig trig rawVal = do
    (lpPad.cloneLens trig.pull) .= val
    fmap Right $ use $ lpPad.cloneLens trig
  where val = (fromIntegral rawVal - fromIntegral (minBound :: Int16)) /
              (fromIntegral (maxBound :: Int16) -
               fromIntegral (minBound :: Int16))

-- Game config

class ButtonLike button where
  pressed :: ALens' Pad button -> Filter
  pressed = not . released

  released :: ALens' Pad button -> Filter
  released = not . pressed

instance ButtonLike Button where
  pressed = Filter . (.butPressed.to id)

instance ButtonLike Dir where
  pressed aLens =
      Filter $ \pad -> pad^.dpad.(pad^.dpad.dir).to direction.to
      (==(pad^.cloneLens aLens.to direction))

onHash :: (Pad -> Int) -> Filter -> LambdaPad user () -> LambdaPad user ()
onHash hashGet filter' act = do
    hashVal <- use $ lpPad.to hashGet
    lpEventFilter %= HM.insertWith (++) hashVal [(filter', act)]

onButton :: ALens' Pad Button -> Filter -> LambdaPad user ()
         -> LambdaPad user ()
onButton aLens = onHash . (cloneLens aLens.to buttonHash)

onButtonPress :: ALens' Pad Button -> LambdaPad user () -> LambdaPad user ()
onButtonPress aLens = onButton aLens (Filter $ cloneLens aLens^.to pressed)

onButtonRelease :: ALens' Pad Button -> LambdaPad user () -> LambdaPad user ()
onButtonRelease aLens = onButton aLens (Filter $ cloneLens aLens^.to released)
{-
  , onDPad
  , onTrigger
  , onAxis
  , addPeriodicCallback
-}

-- Running

withLambdaPad :: LambdaPad user a -> LambdaPadInner user a
withLambdaPad act = do
    lambdaPadData <- liftIO . takeMVar =<< get
    (val, lambdaPadData') <- liftIO $ runStateT act lambdaPadData
    liftIO . flip putMVar lambdaPadData' =<< get
    return val

lambdaPad :: user -> PadConfig user -> LambdaPad user () -> IO ()
lambdaPad userData padConfig gameConfig = do
    SDL.initialize [SDL.InitJoystick]
    numSticks <- SDL.numJoysticks
    joysticks <- SDL.availableJoysticks
    when (numSticks > 0) $ do
      joystick <- SDL.openJoystick $ V.head joysticks
      putStrLn "Starting to listen."
      lambdaPadData <- execStateT gameConfig $ LambdaPadData
          { _lpUserData = userData
          , _lpJoystick = joystick
          , _lpPadConfig = padConfig
          , _lpEventFilter = HM.empty
          , _lpPad = emptyPad
          , _lpOnTick = liftIO . print =<< use lpPad
          , _lpInterval = 1 / 60
          }
      mvarLambdaPadData <- newMVar $ lambdaPadData
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
            mbHash <- case eventPayload of
              SDL.JoyButtonEvent (SDL.JoyButtonEventData
                {SDL.joyButtonEventButton, SDL.joyButtonEventState}) -> do
                  on <- fmap buttonConfig $ use lpPadConfig
                  mbBut <- case joyButtonEventState of
                    0 -> on joyButtonEventButton False
                    1 -> on joyButtonEventButton True
                    _ -> do
                      liftIO $ putStrLn $ -- TODO: actual logging
                          "Unrecognized button state: " ++
                          show joyButtonEventState
                      return Nothing
                  return $ fmap (buttonHash) mbBut
              SDL.JoyHatEvent (SDL.JoyHatEventData
                {SDL.joyHatEventHat, SDL.joyHatEventValue}) -> do
                  on <- fmap dpadConfig $ use lpPadConfig
                  (fmap.fmap) dirHash $ on joyHatEventHat joyHatEventValue
              SDL.JoyAxisEvent (SDL.JoyAxisEventData
                {SDL.joyAxisEventAxis, SDL.joyAxisEventValue}) -> do
                  on <- fmap axisConfig $ use lpPadConfig
                  mbEiAxisTrig <- on joyAxisEventAxis joyAxisEventValue
                  return $ flip fmap mbEiAxisTrig $ \eiAxisTrig -> do
                      case eiAxisTrig of
                        Left axis -> axisHash axis
                        Right trig -> triggerHash trig
              _ -> return Nothing
            eventFilter <- use lpEventFilter
            whenJust (mbHash >>= flip HM.lookup eventFilter) evaluateFilter

evaluateFilter :: [(Filter, LambdaPad user ())] -> LambdaPad user ()
evaluateFilter [] = return ()
evaluateFilter ((filter', act):rest) = do
    doAct <- fmap (runFilter filter') $ use lpPad
    if doAct
      then act
      else evaluateFilter rest

initTickLoop :: MVar (LambdaPadData user) -> IO Stop
initTickLoop mvarLambdaPadData = do
    mvarStop <- newMVar ()
    _ <- SDL.addTimer 0 $ listenTick mvarStop mvarLambdaPadData
    return $ Stop $ stopLoop mvarStop

listenTick :: MVar () -> MVar (LambdaPadData user) -> SDL.TimerCallback
listenTick mvarStop mvarLambdaPadData _ = do
    startTime <- SDL.ticks
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
