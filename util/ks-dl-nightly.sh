#! /bin/bash

# Add the location of the ks-download binaries and scripts to the PATH
PATH="/opt/ks-download/bin:${PATH}"

configDir=/home/ksadmin/.config/ksnitch

# This is where inspections are downloaded and places-matched
# This is a good candidate for backup
workDirParent=/data/ksnitch/download/nc_wake
#workDirParent=/home/dino/dev/ksnitch/data/nc_wake_daily


# When running outside of this zone (or on a system using UTC),
# need this set explicitly for both the `date` binary and ks-dlinsp
export TZ="America/New_York"

workDir=${workDirParent}/nc_wake_$(date +"%Y-%m-%d" --date='2 days ago')


mkdir -p $workDir/{insp,succ,fail}
cd $workDir


# Scrape new inspections from two days ago

ks-dlinsp \
   --insp-source nc_wake \
   --dest-dir insp \
   > ks-dlinsp.log


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
