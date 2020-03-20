#!/usr/bin/env sh

picom &
nitrogen --restore &
xrdb -merge "$HOME/.config/X11/Xresources"
urxvtd -q -o -f &
emacs --daemon &
udiskie --tray --no-config &
volumeicon &
nm-applet &
xfce4-power-manager &
