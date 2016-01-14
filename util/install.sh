#! /bin/bash

set -e

sandboxDir="../cabal-sandbox"
project=ks-download
version=$(perl -n -e '/^version: (.*)/ && print $1' ${project}.cabal)

cabal clean
cabal sandbox init --sandbox=$sandboxDir
cabal install --only-dep
cabal install --prefix=/opt/ksnitch/${project}-${version} --datasubdir=.
