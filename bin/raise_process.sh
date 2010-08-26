#!/bin/bash
# Raises the window of the process whose invocation name matches $1
PIDS=$(wmctrl -lp | cut -c 15- | cut -f 1 -d ' ')
for p in $PIDS
do
    LINE=$(ps $p | cut -c 28- | tail -n 1)
    # if we get a match
    if echo $LINE | grep "$1"
    then
	ID=$(wmctrl -lp | grep $p | cut -c 1-10)
	echo raising $p
	wmctrl -ia $ID
	exit 0;
    fi
done

exit 1;
