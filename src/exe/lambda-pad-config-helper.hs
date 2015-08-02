{-# LANGUAGE NamedFieldPuns #-}
module Main where

import Control.Concurrent ( ThreadId, forkIO, yield )
import Control.Concurrent.MVar
  ( MVar, newMVar, isEmptyMVar, takeMVar, putMVar )

import qualified Data.Vector as V
import qualified SDL

main :: IO ()
main = do
    SDL.initialize [SDL.InitJoystick]
    joysticks <- SDL.availableJoysticks
    if V.length joysticks > 0
      then do
        mapM_ openJoystick $ zip [0 :: Int ..] $ V.toList joysticks
        putStrLn "Displaying events, press enter to stop."
        (mvarStop, _) <- runLoop listen
        _ <- getLine
        stopLoop mvarStop
      else putStrLn "No joysticks found !"
    SDL.quit
  where openJoystick (joyIndex, joyDevice) = do
          putStrLn $ "Openning joystick " ++ show joyIndex ++ " named " ++
              (show $ SDL.joystickDeviceName $ joyDevice)
          _ <- SDL.openJoystick joyDevice
          return ()

runLoop :: IO () -> IO (MVar (), ThreadId)
runLoop act = do
    mvarStop <- newMVar ()
    tid <- forkIO $ loop mvarStop
    return (mvarStop, tid)
  where
    loop :: MVar () -> IO ()
    loop mvarStop = do
        act
        yield
        aStop <- isEmptyMVar mvarStop
        if aStop
          then putMVar mvarStop ()
          else loop mvarStop

stopLoop :: MVar () -> IO ()
stopLoop mvarStop = do
    takeMVar mvarStop -- Signal readLoop
    takeMVar mvarStop -- Wait for readLoop

listen :: IO ()
listen = SDL.waitEventTimeout 1000 >>= maybe (return ()) listen'
  where listen' SDL.Event{SDL.eventPayload} =
          case eventPayload of
            SDL.JoyButtonEvent (SDL.JoyButtonEventData
              { SDL.joyButtonEventButton, SDL.joyButtonEventState
              , SDL.joyButtonEventWhich
              }) ->
                putStrLn $ "On Pad " ++ show joyButtonEventWhich ++
                    " Button " ++ show joyButtonEventButton ++ " is " ++
                    case joyButtonEventState of
                      0 -> "Released"
                      1 -> "Pressed"
                      _ -> "Unrecognized: " ++ show joyButtonEventState
            SDL.JoyHatEvent (SDL.JoyHatEventData
              { SDL.joyHatEventHat, SDL.joyHatEventValue
              , SDL.joyHatEventWhich
              }) ->
                putStrLn $ "On Pad " ++  show joyHatEventWhich ++ " Hat " ++
                    show joyHatEventHat ++ " is " ++ show joyHatEventValue
            SDL.JoyAxisEvent (SDL.JoyAxisEventData
              { SDL.joyAxisEventAxis, SDL.joyAxisEventValue 
              , SDL.joyAxisEventWhich
              }) ->
                putStrLn $ "On Pad " ++ show joyAxisEventWhich ++ " Axis " ++
                    show joyAxisEventAxis ++ " tilted " ++
                    show joyAxisEventValue
            _ -> return ()
