{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
module Game.LambdaPad.Internal where

import Control.Applicative ( Applicative, (<$>) )
import Control.Concurrent
  ( ThreadId, forkIO, yield )
import Control.Concurrent.MVar
  ( MVar, newMVar, isEmptyMVar, takeMVar, putMVar )
import Control.Monad.Reader.Class ( MonadReader, ask, local )
import Control.Monad.State.Strict
    ( State, StateT, evalStateT, execStateT, runState, runStateT )
import Control.Monad.State.Class ( MonadState, get, put )
import Control.Monad.Trans ( MonadIO, liftIO )
import Data.Int ( Int16 )
import Data.Monoid ( Monoid, mempty, mappend, (<>) )
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

newtype Stop = Stop {runStop :: IO ()}

instance Monoid Stop where
  mempty = Stop $ return ()
  mappend stop1 stop2 = Stop $ stop stop1 >> stop stop2

stop :: Stop -> IO ()
stop = runStop

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

data DPad = DPad
    { _dir :: Direction
    , dpadHash :: !Int
    }
makeLenses ''DPad

instance Show DPad where
    showsPrec _ dpad' =
        ("dpad["++) . (dpad'^.dir.to shows) .
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

instance Show Axis where
  showsPrec _ axis = ("axis("++) . (axis^.horiz.to shows) . (","++) .
      (axis^.horiz.to shows) . (')':)

-- | This is the tilt of the axis measured by the clockwise distance around the
-- axis you are starting at up or N.  E.g. N = 0, S = 1/2, W = 1/4, and NE =
-- 7/8.
--
-- A neutral stick has tilt Nothing, but this should not be used to determine
-- whether it has any tilt due to noise.  Check instead whether the push is
-- greater than some small number, e.g. 0.2.
tilt :: Axis -> Maybe Float
tilt axis | x == 0 && y == 0 = Nothing
          | y == 0 = Just $ if x > 0 then 1/4 else 3/4
          | x == 0 = Just $ if y > 0 then 0 else 1/2
          | theta > 0 = Just $ if y > 0 then theta else 0.5 + theta
          | otherwise = Just $ if y > 0 then 1 + theta else 0.5 + theta
  where
    x = axis^.horiz
    y = axis^.vert
    -- Note that x and y are switched.  This equivalent to a flip across x = y.
    -- Yes, y could be 0, but laziness.
    theta = (atan $ x / y) / (2*pi)

-- | This is the amount an axis is displaced from center, where @0.0@ is neutral
-- and @1.0@ is fully displaced.
push :: Axis -> Float
push axis = sqrt $ (axis^.horiz.to sq) + (axis^.vert.to sq)
    where sq x = min 1.0 $ x*x

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

makeFilterOp :: (Bool -> Bool -> Bool)
             -> Filter user -> Filter user -> Filter user
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

type LambdaPadInner user = StateT (LambdaPadData user) IO

type LambdaPadMvar user = StateT (MVar (LambdaPadData user)) IO

newtype LambdaPad user a = LambdaPad { runLambdaPad :: LambdaPadInner user a }
  deriving (Monad, MonadIO, Functor, Applicative)

newtype PadState a = PadState
    { runPadState :: State Pad a }
  deriving (Monad, Functor, Applicative)

newtype GameWriter user a = GameWriter
    { runGameWriter :: LambdaPadInner user a }
  deriving (Monad, Functor, Applicative)

data PadConfig = PadConfig
    { buttonConfig :: Word8 -> ButtonState -> PadState (Maybe Button)
    , dpadConfig :: Word8 -> Word8 -> PadState (Maybe DPad)
    , axisConfig :: Word8 -> Int16 -> PadState (Maybe (Either Axis Trigger))
    }

data GameConfig user = GameConfig
    { newUserData :: IO user
    , onEvents :: GameWriter user ()
    }

data LambdaPadData user = LambdaPadData
    { _lpUserData :: !user
    , _lpJoystick :: SDL.Joystick
    , _lpPad :: !Pad
    , _lpOnTick :: LambdaPad user ()
    , _lpInterval :: Float -- ^ In seconds.
    , _lpPadConfig :: !PadConfig
    , _lpEventFilter :: HM.HashMap Int [(Filter user, LambdaPad user ())]
    }
makeLenses ''LambdaPadData

instance MonadState user (LambdaPad user) where
  get = LambdaPad $ use lpUserData
  put = LambdaPad . (lpUserData.=)

instance MonadReader Pad (LambdaPad user) where
  ask = LambdaPad $ use lpPad
  local f m = LambdaPad $ get >>=
      (liftIO . evalStateT (lpPad %= f >> runLambdaPad m))

isPad :: Filter user -> LambdaPad user Bool
isPad = LambdaPad . flip fmap get . runFilter

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
    , _dpad = DPad C 11
    , _leftTrigger = Trigger 0.0 12
    , _rightTrigger = Trigger 0.0 13
    , _leftStick = Axis 0.0 0.0 14
    , _rightStick = Axis 0.0 0.0 15
    }

-- Pad Config

instance MonadState Pad PadState where
  get = PadState $ get
  put = PadState . put

runLambdaPadState :: PadState a -> LambdaPadInner user a
runLambdaPadState padState = do
    (val, newPad) <- (runState $ runPadState padState) <$> use lpPad
    lpPad .= newPad
    return val

simpleButtonConfig
    :: [(Word8, ALens' Pad Button)]
    -> Word8 -> ButtonState -> PadState (Maybe Button)
simpleButtonConfig rawMapping button state = do
    case HM.lookup button mapping of
      Nothing -> return Nothing
      Just but -> do
        (cloneLens but.buttonState) .= state
        fmap Just $ use $ cloneLens but
  where !mapping = HM.fromList rawMapping 

simpleDPadConfig ::
  Word8 -> [(Word8, Direction)] -> Word8 -> Word8 -> PadState (Maybe DPad)
simpleDPadConfig hatIndex rawMapping hat dirWord =
    if hatIndex == hat
      then do
        case HM.lookup dirWord mapping of
          Nothing -> return Nothing
          Just dir' -> do
            (dpad.dir) .= dir'
            Just <$> use dpad
      else return Nothing
  where !mapping = HM.fromList rawMapping 

simpleAxisConfig
    :: [(Word8, Int16 -> PadState (Either Axis Trigger))]
    -> Word8 -> Int16 -> PadState (Maybe (Either Axis Trigger))
simpleAxisConfig rawMapping axis val =
    case HM.lookup axis mapping of
      Nothing -> return Nothing
      Just axisAct -> Just <$> axisAct val
  where !mapping = HM.fromList rawMapping 

horizAxisConfig :: ALens' Pad Axis -> Int16 -> PadState (Either Axis Trigger)
horizAxisConfig axis rawVal = do
    (cloneLens axis.horiz) .= val
    fmap Left $ use $ cloneLens axis
  where
    val = fromIntegral (if rawVal == minBound then minBound + 1 else rawVal) /
              fromIntegral (maxBound :: Int16)

vertAxisConfig :: ALens' Pad Axis -> Int16 -> PadState (Either Axis Trigger)
vertAxisConfig axis rawVal = do
    (cloneLens axis.vert) .= val
    fmap Left $ use $ cloneLens axis
  where
    val = negate $ fromIntegral
        (1+if rawVal == maxBound then maxBound - 1 else rawVal) /
        fromIntegral (maxBound :: Int16)

triggerConfig :: ALens' Pad Trigger -> Int16 -> PadState (Either Axis Trigger)
triggerConfig trig rawVal = do
    (cloneLens trig.pull) .= val
    fmap Right $ use $ cloneLens trig
  where
    val = (fromIntegral rawVal - fromIntegral (minBound :: Int16)) /
        (fromIntegral (maxBound :: Int16) - fromIntegral (minBound :: Int16))

-- Game config

class WithFilter input a where
  with :: ALens' Pad input -> a -> Filter user

whenPad :: (Pad -> Bool) -> Filter user
whenPad = Filter . flip (^.) . (lpPad.) . to

whenUser :: (user -> Bool) -> Filter user
whenUser userPred = Filter (^.lpUserData.to userPred)

instance WithFilter Button ButtonState where
  with but state = whenPad (^.cloneLens but.buttonState.to (==state))

instance WithFilter DPad Direction where
  with dpad' dir' = whenPad (^.cloneLens dpad'.dir.to (==dir'))

newtype Pull = Pull (Float -> Bool)

instance WithFilter Trigger Pull where
  with trig (Pull pullPred) = whenPad (^.cloneLens trig.pull.to pullPred)

data AxisFilter = Horiz (Float -> Bool)
                | Vert (Float -> Bool)
                | Push (Float -> Bool)
                | Tilt (Float, Float)

axisWith :: ALens' Pad Axis -> (Axis -> Float) -> (Float -> Bool) -> Filter user
axisWith aLens getFloat axisPred = whenPad
    (^.cloneLens aLens.to getFloat.to axisPred)

instance WithFilter Axis AxisFilter where
  with axis (Horiz horizPred) = axisWith axis (^.horiz)  horizPred
  with axis (Vert vertPred) = axisWith axis (^.vert) vertPred
  with axis (Push pushPred) = axisWith axis (^.to push) pushPred
  with axis (Tilt (at, range)) =
      whenPad (^.cloneLens axis.to tilt.to (maybe False inBounds))
    where
      fracPart val = abs $ val - fromIntegral (truncate val :: Int)
      lowerBound = fracPart $ at - (range / 2)
      upperBound = fracPart $ at + (range / 2)
      inBounds tilt'
          | lowerBound < upperBound =
              lowerBound <= tilt' && tilt' < upperBound
          | otherwise =
              upperBound < tilt' && tilt' <= lowerBound

instance WithFilter Axis Direction where
  with axis dir' = case dir' of
      C -> with axis $ Push (<0.2)
      N -> tiltAt 0
      NE -> tiltAt $ 1/8 
      E -> tiltAt $ 1/4
      SE -> tiltAt $ 3/8
      S -> tiltAt $ 1/2
      SW -> tiltAt $ 5/8
      W -> tiltAt $ 3/4
      NW -> tiltAt $ 7/8
    where
      tiltAt at =
          with axis (Push (>=0.25)) && with axis (Tilt (at, 1/8))

onHash :: (a -> Int) -> ALens' Pad a -> Filter user -> LambdaPad user ()
       -> GameWriter user ()
onHash aHash aLens filter' act = GameWriter $ do
    hashVal <- use $ lpPad.cloneLens aLens.to aHash
    lpEventFilter %= HM.insertWith (++) hashVal [(filter', act)]

onButton :: ALens' Pad Button -> Filter user -> LambdaPad user ()
         -> GameWriter user ()
onButton = onHash buttonHash

onButtonPress :: ALens' Pad Button -> Filter user -> LambdaPad user ()
              -> GameWriter user ()
onButtonPress but filter' = onButton but $
    with but Pressed && filter'

onButtonRelease :: ALens' Pad Button -> Filter user -> LambdaPad user ()
                -> GameWriter user ()
onButtonRelease but filter' = onButton but $ 
    with but Released && filter'

onDPad :: Filter user -> LambdaPad user () -> GameWriter user ()
onDPad = onHash dpadHash dpad

onDPadDir :: Direction -> Filter user -> LambdaPad user ()
       -> GameWriter user ()
onDPadDir dir' filter' = onDPad $ with dpad dir' && filter'

onTrigger :: ALens' Pad Trigger -> Filter user -> LambdaPad user ()
         -> GameWriter user ()
onTrigger = onHash triggerHash

onAxis :: ALens' Pad Axis -> Filter user -> LambdaPad user ()
       -> GameWriter user ()
onAxis = onHash axisHash

onTick :: LambdaPad user () -> GameWriter user ()
onTick = GameWriter . (lpOnTick %=) . flip (>>)

-- Running

withLambdaPadInner :: LambdaPadInner user a -> LambdaPadMvar user a
withLambdaPadInner act = do
    lambdaPadData <- liftIO . takeMVar =<< get
    (val, lambdaPadData') <- liftIO $ runStateT act lambdaPadData
    liftIO . flip putMVar lambdaPadData' =<< get
    return val

lambdaPad :: PadConfig -> GameConfig user -> IO Stop
lambdaPad padConfig (GameConfig{newUserData, onEvents}) = do
    SDL.initialize [SDL.InitJoystick]
    numSticks <- SDL.numJoysticks
    joysticks <- SDL.availableJoysticks
    aStop <- if numSticks > 0
      then do
        putStrLn "Acquiring resources."
        joystick <- SDL.openJoystick $ V.head joysticks
        userData <- newUserData
        lambdaPadData <- execStateT (runGameWriter onEvents) $ LambdaPadData
            { _lpUserData = userData
            , _lpJoystick = joystick
            , _lpPadConfig = padConfig
            , _lpEventFilter = HM.empty
            , _lpPad = neutralPad
            , _lpOnTick = return ()
            , _lpInterval = 1 / 60
            }
        mvarLambdaPadData <- newMVar $ lambdaPadData
        putStrLn "Starting to listen."
        eventLoop <- initEventLoop mvarLambdaPadData
        tickLoop <- initTickLoop mvarLambdaPadData
        return $ tickLoop <> eventLoop
      else return mempty
    return $ mappend aStop $ Stop SDL.quit

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
        aStop <- liftIO $ isEmptyMVar mvarStop
        if aStop
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

listenEvent :: LambdaPadMvar user ()
listenEvent = SDL.waitEventTimeout 1000 >>=
    maybe (return ()) (withLambdaPadInner . listen')
  where
    listen' :: SDL.Event -> LambdaPadInner user ()
    listen' SDL.Event{SDL.eventPayload} = do
        mbHash <- case eventPayload of
          SDL.JoyButtonEvent (SDL.JoyButtonEventData
            {SDL.joyButtonEventButton, SDL.joyButtonEventState}) -> do
              on <- buttonConfig <$> use lpPadConfig
              mbBut <- case joyButtonEventState of
                0 -> runLambdaPadState $ on joyButtonEventButton Released
                1 -> runLambdaPadState $ on joyButtonEventButton Pressed
                _ -> do
                  liftIO $ putStrLn $ -- TODO: actual logging
                      "Unrecognized button state: " ++
                      show joyButtonEventState
                  return Nothing
              return $ buttonHash <$> mbBut
          SDL.JoyHatEvent (SDL.JoyHatEventData
            {SDL.joyHatEventHat, SDL.joyHatEventValue}) -> do
              on <- dpadConfig <$> use lpPadConfig
              (fmap.fmap) dpadHash $ runLambdaPadState $
                  on joyHatEventHat joyHatEventValue
          SDL.JoyAxisEvent (SDL.JoyAxisEventData
            {SDL.joyAxisEventAxis, SDL.joyAxisEventValue}) -> do
              on <- axisConfig <$> use lpPadConfig
              mbEiAxisTrig <- runLambdaPadState $
                  on joyAxisEventAxis joyAxisEventValue
              return $ flip fmap mbEiAxisTrig $ \eiAxisTrig -> do
                  case eiAxisTrig of
                    Left axis -> axisHash axis
                    Right trig -> triggerHash trig
          _ -> return Nothing
        eventFilter <- use lpEventFilter
        whenJust (mbHash >>= flip HM.lookup eventFilter) evaluateFilter

evaluateFilter :: [(Filter user, LambdaPad user ())] -> LambdaPadInner user ()
evaluateFilter [] = return ()
evaluateFilter ((filter', act):rest) = do
    doAct <- fmap (runFilter filter') get
    if doAct
      then runLambdaPad act
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
    lambdaPadData' <- execStateT (lambdaPadData^.lpOnTick.to runLambdaPad)
        lambdaPadData
    let interval = lambdaPadData'^.lpInterval
    putMVar mvarLambdaPadData lambdaPadData'

    aStop <- isEmptyMVar mvarStop
    if aStop
      then do
        liftIO $ putMVar mvarStop ()
        return SDL.Cancel
      else do
        endTime <- liftIO $ SDL.ticks
        return $ SDL.Reschedule $
            (floor (interval * 1000) - (endTime - startTime))
