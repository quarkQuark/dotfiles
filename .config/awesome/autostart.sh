#!/usr/bin/env sh

# run runs programs only if they are not already running
function run {
    if ! pgrep $1 > /dev/null ;
    then
        $@&
    fi
}

run picom
run nitrogen --restore
run xcape -e 'Super_L=Control_L|Escape'
run xrdb -merge "$HOME/.config/X11/Xresources"
run urxvtd -q -o -f
run emacs --daemon
run udiskie --tray --no-config
run volumeicon
run nm-applet
run xfce4-power-manager
