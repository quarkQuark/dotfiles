#!/usr/bin/sh

# Command line arguments
MENU_CMD=$1
EDITOR_CMD=$2

# My scripts directory
scripts="$HOME/.scripts/"

# Use $MENU_CMD to choose a file from the scripts directory
choice=`ls $scripts | $MENU_CMD 'Edit script: '`

# If something is chosen (ESC not pressed), then edit it
[ -z $choice ] || $EDITOR_CMD "${scripts}${choice}"
