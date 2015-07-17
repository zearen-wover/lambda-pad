module Game.LambdaPad
  ( lambdaPad, LambdaPad
  -- Pad data structure
  , Direction (C, N, NE, E, SE, S, SW, W, NW)
  , Dir(direction)
  , Button, butPressed
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
  -- Game configuration
  , Filter(Filter)
  , ButtonLike(pressed)
  , onButton
  , onButtonPress
  , onButtonRelease
  , onDPad
  , onTrigger
  , onAxis
  , addPeriodicCallback
  ) where
import Game.LambdaPad.Internal
