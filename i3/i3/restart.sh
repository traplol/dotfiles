#!/bin/bash

pkill xcompmgr
sleep 1
xcompmgr -c -f -n
i3-msg restart

. ~/.backgrounds/background.sh
