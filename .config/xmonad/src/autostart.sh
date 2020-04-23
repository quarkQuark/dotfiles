# Desktop
picom &
nitrogen --restore &

# Settings
xcape -e 'Super_L=Control_L|Escape' &
xrdb -merge "$HOME/.config/X11/Xresources" &

# Daemons
urxvtd -q -o -f &
emacs --daemon &

# Tray
stalonetray --config ~/.config/xmonad/src/stalonetrayrc &
udiskie --tray --no-config &
volumeicon &
nm-applet &
xfce4-power-manager &
