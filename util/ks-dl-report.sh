#! /bin/bash

# To send output via email:
#   ks-dl-report.sh | sed 's/^/  /' | mail -s "ks report $(date +'%Y-%m-%d')" ADDRESS1,ADDRESS2,...
#
# Sample for crontab (note the escaped %s!)
#   45 11 * * *  /opt/ksnitch/ks-download/bin/ks-dl-report.sh | sed 's/^/  /' | mail -s "ks report $(date +'\%Y-\%m-\%d')" ADDRESS1,ADDRESS2,...


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
   echo "Match successes (inspection name -> Places name):"
   echo
   grep -- '->' ks-locate.log | sed 's/\.json//g' | sed -r 's/.*_([^_]+ -> .*)/\1/' | sed -r 's/(.*-> ).*_([^_]+)$/\1\2/'

   echo
   echo "Match failures:"
   echo
   ls -1 fail | sed -r 's/insp_(.*)\.json/\1/g'
}


for ksSource in nc_wake nc_durham nc_orange nc_chatham
do
   report $ksSource
done
