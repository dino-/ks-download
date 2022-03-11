#! /bin/bash

#   ks-dl-report.sh | ks-mail.sh RECIPIENT1,RECIPIENT2...

subject="ks report $(date +'%Y-%m-%d')"

headers=$(cat <<HEADERS
From: noreply@honuapps.com
To: $@
Subject: $subject
Content-Type: text/plain; charset=utf-8
Content-Disposition: inline
HEADERS
)

cat <(echo "$headers") <(echo) - | /usr/sbin/sendmail -t
# For debugging
# cat <(echo "$headers") <(echo) -
