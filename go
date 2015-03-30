#!/bin/bash -e

cabal build
dist/build/quickparse/quickparse data/1.txt