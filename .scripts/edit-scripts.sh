#!/usr/bin/env bash

MENU_CMD=$1
EDITOR_CMD=$2
scripts="$HOME/.scripts/"

choice=$(ls $scripts | $MENU_CMD 'Edit script: ')
$EDITOR_CMD "${scripts}${choice}"
