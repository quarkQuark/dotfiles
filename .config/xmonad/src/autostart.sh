# Desktop
nitrogen --restore &
picom &
redshift-gtk &

# Daemons
emacs --daemon &

# Tray Icons
udiskie --tray --appindicator --no-config &
nm-applet --indicator &
lxqt-powermanagement &

# The following do not yet show in taffybar
#volumeicon &
pasystray &  # Works for IvanMalison
# I still prefer xfce4-power-manager to lxqt-powermanagement,
# but I can't get it to show up in taffybar
#xfce4-power-manager &

# Settings
xrdb -merge "$HOME/.config/X11/Xresources" &
$HOME/.config/xmonad/src/modifiers.sh &
command -v autorandr >/dev/null && autorandr --change &
