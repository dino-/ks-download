#! /bin/bash

# To send output via email:
#   ks-dl-report.sh | mail -s SUBJECT ADDRESS1,ADDRESS2,...


export TZ="America/New_York"


function report {
   ksSource=$1

   workDirParent=/data/ksnitch/download/$ksSource
   workDir=${workDirParent}/${ksSource}_$(date +"%Y-%m-%d")
   cd $workDir

   echo
   echo "------------------------------------------------------------"
   echo -n "Run in directory "
   pwd

   failed=$(ls fail | wc -l)
   succeeded=$(ls succ | wc -l)
   rate=$(dc -e "2k$failed 100*$failed $succeeded+/100r-pq")
   echo
   echo "$rate% match success rate. $succeeded succeeded, $failed failed."

   echo
   echo "Matches:"
   grep -- '->' ks-locate.log
}


for ksSource in nc_wake nc_durham nc_orange nc_chatham
do
   report $ksSource
done
