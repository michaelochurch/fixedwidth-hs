#!/bin/bash -e

cabal build
dist/build/fixedwidth-hs/fixedwidth-hs resources/1.txt
