# Desktop
nitrogen --restore &
picom &
redshift &

# Settings
xcape -e 'Super_L=Control_L|Escape' &
xrdb -merge "$HOME/.config/X11/Xresources" &

# Daemons
emacs --daemon &

# Tray
stalonetray --config ~/.config/xmonad/src/stalonetrayrc &
udiskie --tray --no-config &
volumeicon &
nm-applet &
xfce4-power-manager &
