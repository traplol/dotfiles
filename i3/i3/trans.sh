#!/bin/bash

if [ "$(pgrep xcompmgr)" == "" ] ; then
    xcompmgr -c -f -n
    echo "yes" >> /home/max/trans.out
else
    echo "no" >> /home/max/trans.out
fi
