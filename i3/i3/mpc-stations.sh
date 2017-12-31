#!/bin/bash

case "$1" in
    "voc"* ) echo "Vocaloid Radio"
        mpc clear
        mpc add http://curiosity.shoutca.st:8019/stream
        mpc play
        ;;
    "zemp"* ) echo "Radio Zempire"
        mpc clear
        mpc add http://radio.zempirians.com:8000/RadioZempire.m3u
        mpc play
        ;;
    * ) echo "Don't know \"$1\""
esac
