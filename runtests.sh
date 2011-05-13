#!/bin/bash
cabal configure -ftest
cabal build
./dist/build/test/test $@
