#!/bin/sh

choice=$(ls $HOME/.config/dmenu/ | dmenu -i -p 'Edit script: ')
$1 $2 $3 $4 "$HOME/.config/dmenu/$choice"
