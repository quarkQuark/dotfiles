#! /bin/sh

# System
picom &
nitrogen --restore &
xfce4-panel --restart &

# Settings
xcape -e 'Super_L=Control_L|Escape' &
xrdb -merge ~/.config/X11/Xresources &

# Daemons
urxvtd -q -o -f &
emacs --daemon &
xfsettingsd &

# Tray Icons
udiskie --tray --no-config &
volumeicon &
nm-applet &
xfce4-power-manager &
