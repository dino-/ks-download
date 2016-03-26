#! /bin/bash

ksSource=${1:?Must specify a source}

# Add the location of the ks-download binaries and scripts to the PATH
PATH="/opt/ksnitch/ks-download/bin:${PATH}"

configDir=/home/ksadmin/.config/ksnitch

# This is where inspections are downloaded and places-matched
# This is a good candidate for backup
workDirParent=/data/ksnitch/download/$ksSource
#workDirParent=/home/dino/dev/ksnitch/download/$ksSource


# Need this set explicitly for both the `date` binary below
export TZ="America/New_York"


workDir=${workDirParent}/${ksSource}_$(date +"%Y-%m-%d" --date='2 days ago')


mkdir -p $workDir/{insp,succ,fail}
cd $workDir


# Scrape new inspections from two days ago

ks-dlinsp-cdp $configDir $ksSource insp > ks-dlinsp.log


# Places match the inspections

ks-locate \
   --success-dir succ \
   --fail-dir fail \
   --delete \
   $configDir \
   insp \
   > ks-locate.log


# Import into MongoDB

ks-dbinsert $configDir succ/ > ks-dbinsert.log

# Use these to simulate import success or failure
# comment these lines out if using the above ks-dbinsert command
#echo "Fake ks-dbinsert success" > ks-dbinsert.log; true
#echo "Fake ks-dbinsert failure" > ks-dbinsert.log; false

dbinsertExit=$?

if [ $dbinsertExit != 0 ]
then
   echo "There was a problem importing KS records from ${workDir}/succ/"
   echo "ks-dbinsert exit code: $dbinsertExit"
   echo "see ${workDir}/ks-dbinsert.log"
fi
