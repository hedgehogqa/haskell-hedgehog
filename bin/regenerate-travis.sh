#!/bin/sh -eu

# .travis.yml is generated using the 'haskell-ci' available on Hackage

haskell-ci regenerate --copy-fields=all
