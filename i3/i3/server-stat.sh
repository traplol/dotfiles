#!/bin/bash

if [[ "$1" == "" ]] ; then
    TARGET="192.168.1.10"
else
    TARGET="$1"
fi

ping -c 1 -i .2 $TARGET &> /dev/null
STATUS=$?

if [ $STATUS -ne 0 ] ; then
    echo " DOWN "
    echo " $TARGET: DOWN "
    echo "#ff0000"
else
    echo " UP "
    echo " $TARGET: UP "
    echo "#00ff00"
fi
