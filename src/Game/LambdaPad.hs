module Game.LambdaPad
  ( lambdaPad, LambdaPad
  -- Pad data structure
  , Direction (C, N, NE, E, SE, S, SW, W, NW)
  , Dir(direction)
  , Button, pressed
  , DPad, dir
  , Trigger, pull
  , Axis, horiz, vert, tilt
  , Pad
  , a, b, x, y
  , lb, rb, ls, rs
  , back, start, home
  , dpad, c, n, ne, e, se, s, sw, w, nw
  , leftTrigger, rightTrigger
  , leftStick, rightStick
  -- Pad configuration
  , PadConfig(..)
  , simpleButtonConfig
  , simpleDPadConfig
  , simpleAxisConfig
  , horizAxisConfig
  , vertAxisConfig
  , triggerConfig
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
