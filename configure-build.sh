#! /bin/sh
# Ubuntu uses an outdated version of GHC.  This sets it to a newer one.
GHC=/opt/ghc/7.8.4/bin
CABAL=/opt/cabal/1.22/bin
export PATH=$GHC:$CABAL:$PATH
