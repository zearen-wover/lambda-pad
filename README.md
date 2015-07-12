# lambdapad

A gamepad to keyboard event utility in Haskell.  This only works for Linux
(Note to self, extend `robot` to work on Windows and profit).  This was written
to satisfy my personal needs, but may satisfy the needs of others so I published
it.

Note that this is not for muggles.  All configuration is done in Haskell similar
to XMonad.  Knowledge of Haskell is assumed.  The profiles are launched via the
command line.  I may develop a GTK GUI later, but it's not planned currently.

# Documentation

TODO: Fill this out.

To ease configuration, there's a layer of indirection between the pad
configuration and game configuration.  The actual buttons and axes on the pad
are translated into a set of buttons on the typical controller.  The game
configurations are then written for this logical model.

## Pad profiles

TODO: Fill this out.

This maps the pad hardware to the logical buttons.  There's a utility to help
write these files.

## Game profiles

TODO: Fill this out.

This is a state monad that tracks logical button presses and then sends commands
when certain conditions are met (e.g. A and B are pressed).
