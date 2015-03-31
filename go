#!/bin/bash -e

cabal build
dist/build/quickparse/quickparse resources/1.txt
