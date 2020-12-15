# Desktop
nitrogen --restore &
picom &
redshift-gtk &

# Daemons
#emacs --daemon &

# Tray Icons
status-notifier-watcher # Tracks tray applets for Taffybar
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
$HOME/.scripts/set-modifiers.sh &
