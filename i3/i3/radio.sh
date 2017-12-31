#!/bin/bash

MPC_MSG=$(mpc)
#S=$(echo "$MPC_MSG" | awk '$1 ~ /\[playing\]/ { print $1 }')
STATE=$(echo "$MPC_MSG" | gawk 'match($0, /\[(playing|paused)\]/, m) {print m[1]}')
if [[ "$STATE" == "playing" ]] ; then
    MPC_ACTIVE=true
    if [[ "$1" == "toggle" ]] ; then
        mpc pause
        exit 0
    fi
elif [[ "$STATE" == "paused" ]] ; then
    MPC_ACTIVE=true
    if [[ "$1" == "toggle" ]] ; then
        mpc play
        exit 0
    fi
fi

if [[ "$1" == "info" ]] ; then
    if [[ "$2" == "--i3" ]] ; then
        ROTFILE="/tmp/rot.txt"
        touch "$ROTFILE"
        read ROT < "$ROTFILE"
        (( ROT++ ))
        MAXLEN=50
        echo "$ROT" > "$ROTFILE"
    else
        ROT=0
        MAXLEN=9999
    fi
    
    if [ $MPC_ACTIVE ] ; then
        SONG=$(echo "$MPC_MSG" | gawk 'NR==1{print $0}')
        echo "$(~/.i3/rotate.rb $ROT $MAXLEN "$SONG")"
    else
        ~/.i3/spotify.py --format=' %(artist)s - %(title)s ' --color=#93a1a1 --rotate=$ROT --maxlen=$MAXLEN
    fi
    exit 0
fi

if [[ "$1" == "mpc" || "$1" == "mpd" ]] ; then
    dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.Stop
    mpc play

elif [[ "$1" == "spotify" ]] ; then
    dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.Play
    mpc stop

elif [[ "$1" == "toggle" ]] ; then
    dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.PlayPause

elif [[ "$1" == "stop" ]] ; then
    dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.Stop
    mpc stop

elif [[ "$1" == "next" && ! $MPC_ACTIVE ]] ; then
    dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.Next

elif [[ "$1" == "prev" && ! $MPC_ACTIVE ]] ; then
    dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.Previous

fi
