#!/bin/bash

case "$1" in
    "voc"* ) echo "Vocaloid Radio"
        mpc clear
        mpc add http://curiosity.shoutca.st:8019/stream
        mpc play
        ;;
    * ) echo "Don't know \"$1\""
esac
