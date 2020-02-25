#! /bin/sh

# Settings
xcape -e 'Super_L=Control_L|Escape' &
xrdb -merge ~/.config/X11/Xresources &

# Daemons
urxvtd -q -o -f &
emacs --daemon &
