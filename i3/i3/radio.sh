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
    OUTPUT="$(~/.i3/rotate.rb $ROT $MAXLEN "$SONG")"
else
    OUTPUT="$(~/.i3/spotify.py --format=' %(artist)s - %(title)s ' --rotate=$ROT --maxlen=$MAXLEN --single)"
fi


if [[ "$1" == "mpc" || "$1" == "mpd" ]] ; then
    mpc play
elif [[ "$1" == "spotify" ]] ; then
    SPOTIFY_MSG="Play"
    mpc stop
elif [[ "$1" == "toggle" || "$1" == "2" ]] ; then
    SPOTIFY_MSG="PlayPause"
elif [[ "$1" == "stop" ]] ; then
    SPOTIFY_MSG="Stop"
    mpc stop
elif [[ "$1" == "next" || "$1" == "3" && ! $MPC_ACTIVE ]] ; then
    SPOTIFY_MSG="Next"
elif [[ "$1" == "prev" || "$1" == "1" && ! $MPC_ACTIVE ]] ; then
    SPOTIFY_MSG="Previous"
fi

if [[ "$SPOTIFY_MSG" != "" ]] ; then
    dbus-send --type=method_call \
              --reply-timeout=5000 \
              --dest=org.mpris.MediaPlayer2.spotify \
              /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.$SPOTIFY_MSG
fi

echo "$OUTPUT"
