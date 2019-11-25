#! /bin/sh

picom &
xcape -e 'Super_L=Control_L|Escape' &
xrdb -merge ~/.config/X11/Xresources &

udiskie --tray --no-config &
volumeicon &
nm-applet &
xfce4-power-manager &

nitrogen --restore &
~/.config/polybar/launch.sh

urxvtd -q -o -f &
emacs --daemon &
