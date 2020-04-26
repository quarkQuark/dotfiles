#!/usr/bin/env bash

MENU_CMD=$1
EDITOR_CMD=$2

choice=$(ls $HOME/.config/dmenu/ | $MENU_CMD 'Edit script: ')
$EDITOR_CMD "$HOME/.config/dmenu/$choice"
