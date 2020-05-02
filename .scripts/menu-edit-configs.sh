#!/usr/bin/sh
# Adapted from https://www.gitlab.com/dwt1/dotfiles/-/blob/master/.dmenu/dmenu-edit-configs.sh

# Command line arguments
MENU_CMD=$1
EDITOR_CMD=$2

options="alacritty
awesome
bashrc
bash_aliases
dmenu/edit-configs
nvim/conf
nvim/init
nvim/keys
nvim/plug
nvim/ui
openbox/rc
openbox/autostart
picom
profile
qutebrowser
spacemacs
xmobar
xmonad.hs
xmonad/autostart
xresources
quit"

choice=`echo "$options" | $MENU_CMD "Edit config file: "`

case "$choice" in
    quit)
        exit 1
    ;;
    alacritty)
        file="$HOME/.config/alacritty/base.yml"
    ;;
    awesome)
        file="$HOME/.config/awesome/rc.lua"
    ;;
    bashrc)
        file="$HOME/.bashrc"
    ;;
    bash_aliases)
        file="$HOME/.bash_aliases"
    ;;
    dmenu/edit-configs)
        file="$HOME/.config/dmenu/edit-configs.sh"
    ;;
    nvim/conf)
        file="$HOME/.config/nvim/conf.vim"
    ;;
    nvim/init)
        file="$HOME/.config/nvim/init.vim"
    ;;
    nvim/keys)
        file="$HOME/.config/nvim/keys.vim"
    ;;
    nvim/plug)
        file="$HOME/.config/nvim/plug.vim"
    ;;
    nvim/ui)
        file="$HOME/.config/nvim/ui.vim"
    ;;
    openbox/rc)
        file="$HOME/.config/openbox/rc.xml"
    ;;
    openbox/autostart)
        file="$HOME/.config/openbox/autostart"
    ;;
    picom)
        file="$HOME/.config/picom/picom.conf"
    ;;
    profile)
        file="$HOME/.profile"
    ;;
    qutebrowser)
        file="$HOME/.config/qutebrowser/config.py"
    ;;
    spacemacs)
        file="$HOME/.spacemacs.d/init.el"
    ;;
    xmobar)
        file="$HOME/.config/xmonad/src/xmobarrc.hs"
    ;;
    xmonad.hs)
        file="$HOME/.config/xmonad/src/xmonad.hs"
    ;;
    xmonad/autostart)
        file="$HOME/.config/xmonad/src/autostart.sh"
    ;;
    xresources)
        file="$HOME/.config/X11/Xresources"
    ;;
    *)
        exit 1
    ;;
esac

# Using the command line argument(s) as the editor command
$EDITOR_CMD "$file"
