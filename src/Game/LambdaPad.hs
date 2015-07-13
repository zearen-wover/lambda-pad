module Game.LambdaPad
  ( lambdaPad, LambdaPad
  -- Pad data structure
  , Dir (N, NE, E, SE, S, SW, W, NW)
  , Button, pressed
  , DPad, dir
  , Trigger, pull
  , Axis, horiz, vert, tilt
  , Pad
  , a, b, x, y
  , lb, rb, ls, rs
  , back, start, home
  , dpad  --, n, ne, e, se, s, sw, w, nw
  , leftTrigger, rightTrigger
  , leftStick, rightStick
  -- Pad configuration
  , PadConfig(..) 
  , simpleButtonConfig
  , simpleHatConfig
  -- Events
  {-
  , Event
  , A, B, X, Y
  , LB, LS, RB, RS
  , Back, Start, Home
  , LeftStick, RightStick
  , LeftTrigger, RightTrigger
  -}
  ) where
import Game.LambdaPad.Internal
