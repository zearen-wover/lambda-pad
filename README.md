# lambdapad

This allows a user to control the keyboard and mouse using a game pad (and maybe
do other stuff to !)  This is not for muggles.  All configuration is via Haskell
configuration files powered by Dyre.  It's launched via the command line and has
no graphical UI.

# Using

## Building

If for some strange reason you want to build this, first build
[lambda-pad-core](http://github.com/zearen-wover/lambda-pad-core.git), then

    git clone http://github.com/zearen-wover/lambda-pad.git
    cd lambda-pad
    cabal configure
    cabal build
    cabal install

## Configuring

The configuration is found along with your other XDG configurations.  This is
typically `$HOME/.config/lambda-pad/lambda-pad.hs`.  The typical configuration
just overrides the `gameConfigs` field of `defaultLambdaPadConfig`.  See
[lambda-pad-games](http://github.com/zearen-wover/lambda-pad-core.git) foe a
walk through of configuring.

TODO: Provide a from scratch walkthrough instead.

## Running

All games have an associated name that can be used when launching lambda-pad
set via the -g flag.

    lambda-pad -g minecraft

This flag is required unless `defaultGame` is specified in the config.

TODO: Describe other flags.

TODO: Turn GameConfigs into subcommands and allow users to define their own
flags.

# Documentation

To ease configuration, there's a layer of indirection between the pad
configuration and game configuration.  The actual buttons and axes on the pad
are translated into a set of buttons on the typical controller which are given
symbols.  The game configurations are then based on events and conditions of
these symbols.

## Core

TODO: Fill this out.

The core contains lenses and data types that form a logical representation of a
typical game pad.  These are used throughout the application as setters,
getters, and general symbols for the actual entity on the game pad.

## Game profiles

TODO: Fill this out.

A game config contains information about how to construct it's state, some
metadata, and most importantly, and `onEvents` field that takes `GameWriter`
monad.  This allows the user to add watchers with filters along with a
`LambdaPad` callback that's a reader of the game pad state

## Pad profiles

TODO: Fill this out.

This maps the pad hardware to the logical buttons.

TODO: Write a utility to help write these files.

