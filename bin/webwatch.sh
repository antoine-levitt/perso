#!/bin/bash

# web watch

URL="$1"
NAME="$2"

wget -q "$URL" -O "/tmp/$NAME.new"

if [ ! -f "/tmp/$NAME.ref" ]; then
    mv "/tmp/$NAME.new" "/tmp/$NAME.ref"
elif [ "$(cat /tmp/$NAME.ref | md5sum)" != "$(cat /tmp/$NAME.new | md5sum)" ]; then
    gnome-osd-client "New changes"
    mv "/tmp/$NAME.new" "/tmp/$NAME.ref"
else
    rm "/tmp/$NAME.new"
    echo "NOPE"
    sleep 5
fi
