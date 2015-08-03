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
[lambda-pad-games](http://github.com/zearen-wover/lambda-pad-core.git) for a
walk through of configuring.

TODO: Provide a from scratch walkthrough instead.

## Running

All games have an associated name that can be used when launching lambda-pad
using the package's short name.

    lambda-pad minecraft

This flag is required unless `defaultGame` is specified in the config.

TODO: Describe other flags and user flags.

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

## Game packages

TODO: Fill this out.

A final `PackagedGameConfig` is `package`ed from a `PackageConfig`.  These
contain some metadata, a flag parser,  a `packageGameConfig` field that
constructs a `GameConfig` from the flags.  A game config has knowledge about how
to construct and destruct its state, as well as an `onEvents` field that
contains a `GameWriter` monad.  This allows the user to add watchers with
filters along with a `LambdaPad` callback that's a reader of the game pad state

## Pad configs

TODO: Fill this out.

This maps the pad hardware to the logical buttons.  With any luck, you'll never
have to make one of these as I fill in default pad configs.  If you do, please
it contribute back to
[lambda-pad-core](http://github.com/zearen-wover/lambda-pad-core.git) :)

There's a utility included in this called `lambda-pad-config-helper` that
displays raw events to help write a `PadConfig` if you really have to.
