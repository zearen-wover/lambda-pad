{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
module Game.LambdaPad.Internal where

import Control.Concurrent
  ( ThreadId, forkIO, yield )
import Control.Concurrent.MVar
  ( MVar, newMVar, isEmptyMVar, takeMVar, putMVar )
import Control.Monad ( when )
import Control.Monad.State.Strict
    ( StateT, evalStateT, execStateT, runStateT, get )
import Control.Monad.Trans ( MonadIO, liftIO )
import Data.Int ( Int16 )
import Data.Word ( Word8 )

import Data.Algebra.Boolean ( Boolean(..) )
import Control.Lens
    ( ALens', (.=), (%=), (^.), to, use, cloneLens )
import Prelude hiding ( (&&), (||), not )
import Control.Lens.TH ( makeLenses )

import qualified Data.HashMap.Strict as HM
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

data ButtonState = Pressed | Released
  deriving (Show, Eq)

data Button = Button
    { _buttonState :: !ButtonState
    , buttonHash :: !Int
    }
makeLenses ''Button

instance Show Button where 
  show but = case but^.buttonState of
      Pressed -> "but(*)"
      Released -> "but( )"

data Direction = C | N | NE | E | SE | S | SW | W | NW
  deriving (Show, Eq)

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
  { _trigPull :: !Float
  , triggerHash :: !Int
  }
makeLenses ''Trigger

instance Show Trigger where
  showsPrec _ trig = ("trig("++) . (trig^.trigPull.to shows) . (')':)

data Axis = Axis
    { _horiz :: !Float
    , _vert :: !Float
    , axisHash :: !Int
    }
makeLenses ''Axis

instance Show Axis where
  showsPrec _ axis = ("axis("++) . (axis^.horiz.to shows) . (","++) .
      (axis^.horiz.to shows) . (')':)

-- | This is the tilt of the axis measured by the clockwise distance around the
-- axis you are starting at up or N.  E.g. N = 0, S = 1/2, W = 1/4, and NE =
-- 7/8.
--
-- A neutral stick has tilt Nothing, but this should not be used to determine
-- whether it has any tilt due to noise.  Check instead whether the pull is
-- greater than some small number, e.g. 0.25.
tilt :: Axis -> Maybe Float
tilt axis | x == 0 && y == 0 = Nothing
          | y == 0 = Just if x > 0 then 1/4 else 3/4
          | x == 0 = Just if y > 0 then 0 else 1/2
          | theta > 0 = if y > 0 then theta else 0.5 + theta
          | otherwise = if y > 0 then 1 + theta else 0.5 + theta
  where
    x = axis^.horiz
    y = axis^.vert
    -- Note that x and y are switched.  This equivalent to a flip across x = y.
    -- Yes, y could be 0, but laziness.
    theta = (atan $ x / y) / (2*pi)

-- | This is a control that has a partial value as a deviation from neutrail,
-- i.e. triggers and axes.  It's a value from 0 to 1 where 0 is neutral and 1 is
-- fully displaced.
class HasPull p where
  pull :: p -> Float

instance HasPull Trigger where
  pull = (^._trigPull)

instance HasPull Axis where
  pull axis = sqrt $ (axis^.horiz.to sq) + (axis.horiz.to sq)
    where sq x = x*x

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

newtype Filter user = Filter { runFilter :: LambdaPadData user -> Bool }

makeFilterOp :: (Bool -> Bool -> Bool) -> Filter -> Filter -> Filter
makeFilterOp op left right = Filter $ \tf ->
    runFilter left  tf `op` runFilter right tf

instance Boolean (Filter user) where
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
    { buttonConfig :: Word8 -> ButtonState -> LambdaPad user (Maybe Button)
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
    , _lpEventFilter :: HM.HashMap Int [(Filter user, LambdaPad user ())]
    }
makeLenses ''LambdaPadData

neutralDPad :: DPad
neutralDPad = DPad
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

neutralPad :: Pad
neutralPad = Pad
    { _a = Button Released 0
    , _b = Button Released 1
    , _x = Button Released 2
    , _y = Button Released 3
    , _lb = Button Released 4
    , _rb = Button Released 5
    , _rs = Button Released 6
    , _ls = Button Released 7
    , _start = Button Released 8
    , _back = Button Released 9
    , _home = Button Released 10
    , _dpad = neutralDPad
    , _leftTrigger = Trigger 0.0 20
    , _rightTrigger = Trigger 0.0 21
    , _leftStick = Axis 0.0 0.0 22
    , _rightStick = Axis 0.0 0.0 23
    }

simpleButtonConfig
    :: [(Word8, ALens' Pad Button)]
    -> Word8 -> ButtonState -> LambdaPad user (Maybe Button)
simpleButtonConfig rawMapping button state = do
    case HM.lookup button mapping of
      Nothing -> return Nothing
      Just but -> do
        (lpPad.cloneLens but.buttonState) .= state
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
  where
    val = fromIntegral (if rawVal == minBound then minBound + 1 else rawVal) /
              fromIntegral (maxBound :: Int16)

vertAxisConfig :: ALens' Pad Axis -> Int16
               -> LambdaPad user (Either Axis Trigger)
vertAxisConfig axis rawVal = do
    (lpPad.cloneLens axis.vert) .= val
    fmap Left $ use $  lpPad.cloneLens axis
  where
    val = negate $ fromIntegral
        (1+if rawVal == maxBound then maxBound - 1 else rawVal) /
        fromIntegral (maxBound :: Int16)

triggerConfig :: ALens' Pad Trigger -> Int16
              -> LambdaPad user (Either Axis Trigger)
triggerConfig trig rawVal = do
    (lpPad.cloneLens trig.trigPull) .= val
    fmap Right $ use $ lpPad.cloneLens trig
  where
    val = (fromIntegral rawVal - fromIntegral (minBound :: Int16)) /
        (fromIntegral (maxBound :: Int16) - fromIntegral (minBound :: Int16))

-- Game config

class WithFilter input a where
  with :: ALens' Pad input -> a -> Filter user

whenPad :: (Pad -> Bool) -> Filter user
whenPad = Filter . flip (^.) . (lpPad.)

whenUser :: (user -> Bool) -> Filter user
whenUser pred = Filter (^.lpUserData.to pred)

instance WithFilter Button ButtonState where
  with but state = whenPad (^.cloneLens but.buttonState.to (==state))

instance WithFilter DPad Direction where
  with dpadLens direction' = whenPad $ \pad ->
      let dpad' = pad^.cloneLens dpadLens
      in dpad'^.cloneLens (dpad'.dir).to direction.to (==direction')

instance WithFilter Trigger (Float -> Bool) where
  with trig pullPred = whenPad (^.cloneLens trig.to pull.to pullPred)

data AxisFilter = Horiz (Float -> Bool)
                | Vert (Float -> Bool)
                | Pull (Float -> Bool)
                | TiltAt !Float !Float

axisWith :: ALens' Pad Axis -> (Axis -> Float) -> (Float -> Bool) -> Filter user
axisWith aLens getFloat pred = whenPad (^.cloneLens aLens.to getFloat.to pred)

instance WithFilter Axis AxisFilter where
  with axis (Horiz pred) = axisWith axis (^.horiz)  pred
  with axis (Vert pred) = axisWith axis (^.vert)  pred
  with axis (Pull pred) = axisWith axis pull  pred
  with axis (Tilt at range) =
      whenPad (^.cloneLens axis.to tilt.to (maybe False inBounds))
    where
      fracPart x = abs $ x - fromIntegral (truncate x)
      lowerBound = fracPart $ at - (range / 2)
      upperBound = fracPart $ at + (range / 2)
      inBounds tilt'
          | lowerBound < upperBound =
              lowerBound <= tilt && tilt < upperBound
          | otherwise =
              upperBound < tilt && tilt <= lowerBound

instance WithFilter Axis Direction where
  with axis direction' = case direction' of
      C -> with axis $ Pull (<0.25)
      N -> tiltAt 0
      NE -> tiltAt $ 1/8 
      E -> TiltAt $ 1/4
      SE -> tiltAt $ 3/8
      S -> tiltAt $ 1/2
      SW -> tiltAt $ 5/8
      W -> tiltAt $ 3/4
      NW -> tiltAt $ 7/8
    where
      tiltAt at =
          with axis (Pull (>=0.25)) && with axis (TiltAt at 1/8)

onHash :: (a -> Int) -> ALens' Pad a -> Filter user -> LambdaPad user ()
       -> LambdaPad user ()
onHash aHash aLens filter' act = do
    hashVal <- use $ lpPad.cloneLens aLens.to aHash
    lpEventFilter %= HM.insertWith (++) hashVal [(filter', act)]

onButton :: ALens' Pad Button -> Filter user -> LambdaPad user ()
         -> LambdaPad user ()
onButton = onHash buttonHash

onButtonPress :: ALens' Pad Button -> Filter user -> LambdaPad user ()
              -> LambdaPad user ()
onButtonPress but filter' = onButton but $
    with but Pressed && filter'

onButtonRelease :: ALens' Pad Button -> Filter user -> LambdaPad user ()
                -> LambdaPad user ()
onButtonRelease but filter' = onButton but $ 
    with but Released && filter'

onDPad :: ALens' DPad Dir -> Filter user -> LambdaPad user ()
       -> LambdaPad user ()
onDPad = onHash dirHash . (dpad.)

onTrigger :: ALens' Pad Trigger -> Filter user -> LambdaPad user ()
         -> LambdaPad user ()
onTrigger = onHash triggerHash

onAxis :: ALens' Pad Axis -> Filter user -> LambdaPad user ()
       -> LambdaPad user ()
onAxis = onHash axisHash

onTick :: LambdaPad user () -> LambdaPad user ()
onTick = (lpOnTick %=) . flip (>>)

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
          , _lpPad = neutralPad
          , _lpOnTick = return ()
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
  where
    loopIn :: MonadIO io => MVar () -> LoopIn io
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
  where
    listen' :: SDL.Event -> LambdaPad user ()
    listen' SDL.Event{SDL.eventPayload} = do
        mbHash <- case eventPayload of
          SDL.JoyButtonEvent (SDL.JoyButtonEventData
            {SDL.joyButtonEventButton, SDL.joyButtonEventState}) -> do
              on <- fmap buttonConfig $ use lpPadConfig
              mbBut <- case joyButtonEventState of
                0 -> on joyButtonEventButton Released
                1 -> on joyButtonEventButton Pressed
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
