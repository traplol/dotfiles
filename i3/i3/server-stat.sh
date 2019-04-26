#!/bin/bash

if [[ "$1" != "" ]] ; then
    TARGET="$1"
else
    TARGET="127.0.0.1"
fi

if [[ "$2" != "" ]] ; then
    UP="$2"
else
    UP="UP"
fi

if [[ "$3" != "" ]] ; then
    DOWN="$3"
else
    DOWN="DOWN"
fi

ping -c 1 -i .5 $TARGET &> /dev/null
STATUS=$?

if [ $STATUS -ne 0 ] ; then
    echo "$DOWN"
    echo "$TARGET: $DOWN"
    echo "#ff0000"
else
    echo "$UP"
    echo "$TARGET: $UP"
    echo "#00ff00"
fi
