#! /bin/bash

# Location of the ks-download binaries and scripts
# Needed for the cron environment
binPrefix=/home/dino/bin

# Make this a directory that's backed-up
workDirParent=/home/dino/dev/kitchensnitch/data/nc-wake_daily
#workDirParent=/var/tmp

workDir=${workDirParent}/nc-wake_$(date +"%Y-%m-%d" --date='yesterday')

PATH=$binPrefix:"${PATH}"


mkdir -p $workDir/{insp,succ,fail}
cd $workDir


# Scrape new inspections for yesterday

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


# Import into Couchbase
