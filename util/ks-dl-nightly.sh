#! /bin/bash

# Location of the ks-download binaries and scripts
# Needed for the cron environment
binDir=/opt/ks-download/bin
#binDir=/home/dino/bin

# Make this a directory that's backed-up
workDirParent=/var/local/kitchensnitch/nc-wake_daily
#workDirParent=/home/dino/dev/kitchensnitch/data/nc-wake_daily

# Couchbase password
couchPassword=COUCH_PASSWORD


# When running outside of this zone (or on a system using UTC),
# need this set explicitly for both the `date` binary and ks-dlinsp
export TZ="America/New_York"

workDir=${workDirParent}/nc-wake_$(date +"%Y-%m-%d" --date='2 days ago')

PATH=$binDir:"${PATH}"


mkdir -p $workDir/{insp,succ,fail}
cd $workDir


# Scrape new inspections from two days ago

ks-dlinsp \
   --insp-source nc_wake \
   --dest-dir insp \
   > ks-dlinsp.log


# Places match the inspections

ks-locate \
   --conf-dir /home/dino/.ksdl \
   --success-dir succ \
   --fail-dir fail \
   --delete \
   insp \
   > ks-locate.log


# Import into MongoDB

#ks-dbinsert --conf-dir /home/dino/.ksdl succ/ > ks-mongo-import.log

# Use these to simulate import success or failure
# comment these lines out if using the above ks-dbinsert command
echo "Fake ks-dbinsert success" > ks-mongo-import.log; true
#echo "Fake ks-dbinsert failure" > ks-mongo-import.log; false

dbinsertExit=$?

if [ $dbinsertExit != 0 ]
then
   echo "There was a problem importing KS records from ${workDir}/succ/"
   echo "ks-dbinsert exit code: $dbinsertExit"
   echo "see ${workDir}/ks-mongo-import.log"
fi
