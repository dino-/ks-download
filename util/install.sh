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
   -l, --link  Create symlink in $installDir
   -h, --help  This help info

USAGE
}


# arg parsing

getoptResults=$(getopt -o lh --long link,help -n $basename -- "$@")

if [ $? != 0 ]; then usage; exit 1; fi

# Note the quotes around '$getoptResults': they are essential!
eval set -- "$getoptResults"

optLink=false
optHelp=false

while true ; do
   case "$1" in
      -l|--link) optLink=true; shift;;
      -h|--help) optHelp=true; shift;;
      --) shift; break;;
   esac
done

if $optHelp; then usage; exit 0; fi


cabal clean
cabal sandbox init --sandbox=$sandboxDir
cabal install --only-dep
cabal install --prefix=$installDir/${project}-${version} --datasubdir=.

if $optLink
then
   cd $installDir
   rm -f $project
   ln -s ${project}-${version} $project
fi
