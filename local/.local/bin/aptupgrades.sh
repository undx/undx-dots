#!/usr/bin/env sh
ipurl="icanhazip.com"
if   hash dig  2>/dev/null; then cmd="dig +short myip.opendns.com @resolver1.opendns.com"; ipurl=""
elif hash curl 2>/dev/null; then cmd="curl"
elif hash wget 2>/dev/null; then cmd="wget -qO-"
else echo "*** No app (dig,curl,wget) found - exiting ***"; exit 2; fi
[ -n "$1" ] && echo -n "$cmd $ipurl - "

apts=$(apt upgrade -s)
echo $apts

eval $cmd $ipurl || ret=$?
exit $ret

