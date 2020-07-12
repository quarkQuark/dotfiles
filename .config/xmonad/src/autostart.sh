# Watch for applications that want to use the system tray (for taffybar)
# Part of the haskell package status-notifier-item
status-notifier-watcher &

# Settings
xcape -e 'Super_L=Control_L|Escape' &
xrdb -merge "$HOME/.config/X11/Xresources" &

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
