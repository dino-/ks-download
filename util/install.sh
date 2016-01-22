#! /bin/bash

set -e

installDir="/opt/ksnitch"
sandboxDir="../cabal-sandbox"
project="ks-download"
version=$(perl -n -e '/^version: (.*)/ && print $1' ${project}.cabal)


basename=$(basename $0)

function usage {
   cat <<USAGE
$basename - Build and install $project

usage:
   $basename [OPTIONS]

options:
   -h, --help  This help info
   -l, --link  Create symlink in $installDir

USAGE

   exit 0
}


dolink=false

case "$1" in
   -h|--help) usage ;;
   -l|--link) dolink=true ;;
esac


cabal clean
cabal sandbox init --sandbox=$sandboxDir
cabal install --only-dep
cabal install --prefix=$installDir/${project}-${version} --datasubdir=.

if $dolink
then
   cd $installDir
   rm -f $project
   ln -s ${project}-${version} $project
fi
