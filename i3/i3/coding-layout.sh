#!/bin/bash

# usage: ./script DIRECTORY WORKSPACE

DIRECTORY="${1:-$HOME}"
DIRECTORY=$(readlink -f "$DIRECTORY")
WORKSPACE=$2

if [ ! -z "$WORKSPACE" ]; then
   i3-msg "workspace $WORKSPACE"
fi
emacsclient -c -n "$DIRECTORY" && \
google-chrome && \
gnome-terminal --working-directory="$DIRECTORY" && \
gnome-terminal --working-directory="$DIRECTORY" && \
i3-msg 'layout toggle split' && \
i3-msg 'focus up' && \
i3-msg 'focus up' && \
i3-msg 'move left' && \
i3-msg 'focus right' && \
i3-msg 'focus up' && \
i3-msg 'move left' && \
i3-msg 'resize grow width 6 px or 6 ppt'
