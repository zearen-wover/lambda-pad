module Game.LambdaPad
  ( lambdaPad, LambdaPad
  -- Pad data structure
  , Direction (C, N, NE, E, SE, S, SW, W, NW)
  , Dir(direction)
  , ButtonState(Pressed, Released)
  , Button, buttonState
  , DPad, dir
  , Trigger, trigPull
  , Axis, horiz, vert
  , HasPull(pull)
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
  , Filter(Filter, runFilter)
  , FilterWith(with)
  , AxisFilter
  , onButton
  , onButtonPress
  , onButtonRelease
  , onDPad
  , onTrigger
  , onAxis
  , onTick
  ) where

import Game.LambdaPad.Internal
