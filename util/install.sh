#! /bin/bash

set -e

defaultInstallDir="/opt/ksnitch"
sandboxDir="../cabal-sandbox"
project="ks-download"
version=$(perl -n -e '/^version: (.*)/ && print $1' ${project}.cabal)

basename=$(basename $0)


function usage {
   cat <<USAGE
$basename - Build and install $project

usage:
   $basename [OPTIONS] [DIR]

options:
   -C, --no-clean  Do not do 'clean' build, see below
   -l, --link      Create symlink in DIR
   -h, --help      This help info

DIR is the directory to install into. Defaults to $defaultInstallDir

A 'clean' build will perform 'cabal clean', 'cabal sandbox init...', and 'cabal install --only-dep' before the build of this project. Not always necessary.

Part of the Kitchen Snitch project
2016-04-30  Dino Morelli <dino@ui3.info>

USAGE
}


# arg parsing

getoptResults=$(getopt -o Clh --long no-clean,link,help -n $basename -- "$@")

if [ $? != 0 ]; then usage; exit 1; fi

# Note the quotes around '$getoptResults': they are essential!
eval set -- "$getoptResults"

optClean=true
optLink=false
optHelp=false

while true ; do
   case "$1" in
      -C|--no-clean) optClean=false; shift;;
      -l|--link) optLink=true; shift;;
      -h|--help) optHelp=true; shift;;
      --) shift; break;;
   esac
done

if $optHelp; then usage; exit 0; fi

installDir=${1:-$defaultInstallDir}

if $optClean
then
   cabal clean
   cabal sandbox init --sandbox=$sandboxDir
   cabal install --only-dep
fi
cabal install --prefix=$installDir/${project}-${version} --datasubdir=.

if $optLink
then
   cd $installDir
   rm -f $project
   ln -s ${project}-${version} $project
fi
