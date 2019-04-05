#!/bin/bash

if [ "$(pgrep xcompmgr)" == "" ] ; then
    xcompmgr -c -f -n
fi
