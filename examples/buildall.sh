#!/bin/sh

if [ -d ../.cabal-sandbox ]; then
    cabal sandbox init --sandbox=../.cabal-sandbox
    alias build='cabal exec ghc -- --make'
else
    alias build='ghc --make'
fi

for f in [a-z]*.hs; do
    build "$f" || exit 1
done
