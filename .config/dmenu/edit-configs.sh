#!/bin/sh
# Adapted from https://www.gitlab.com/dwt1/dotfiles/-/blob/master/.dmenu/dmenu-edit-configs.sh

declare options=(
"bashrc
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
xmonad/kde-autostart
xresources
quit")

choice=$(echo -e "${options[@]}" | dmenu -i -p 'Edit config file: ')

case "$choice" in
    quit)
        exit 1
    ;;
    bashrc)
        choice="$HOME/.bashrc"
    ;;
    bash_aliases)
        choice="$HOME/.bash_aliases"
    ;;
    dmenu/edit-configs)
        choice="$HOME/.config/dmenu/edit-configs.sh"
    ;;
    nvim/conf)
        choice="$HOME/.config/nvim/conf.vim"
    ;;
    nvim/init)
        choice="$HOME/.config/nvim/init.vim"
    ;;
    nvim/keys)
        choice="$HOME/.config/nvim/keys.vim"
    ;;
    nvim/plug)
        choice="$HOME/.config/nvim/plug.vim"
    ;;
    nvim/ui)
        choice="$HOME/.config/nvim/ui.vim"
    ;;
    openbox/rc)
        choice="$HOME/.config/openbox/rc.xml"
    ;;
    openbox/autostart)
        choice="$HOME/.config/openbox/autostart"
    ;;
    picom)
        choice="$HOME/.config/picom/picom.conf"
    ;;
    profile)
        choice="$HOME/.profile"
    ;;
    qutebrowser)
        choice="$HOME/.config/qutebrowser/config.py"
    ;;
    spacemacs)
        choice="$HOME/.spacemacs.d/init.el"
    ;;
    xmobar)
        choice="$HOME/.xmonad/xmobarrc.hs"
    ;;
    xmonad.hs)
        choice="$HOME/.xmonad/xmonad.hs"
    ;;
    xmonad/kde-autostart)
        choice="$HOME/.xmonad/kde-autostart.sh"
    ;;
    xresources)
        choice="$HOME/.config/X11/Xresources"
    ;;
    *)
        exit 1
    ;;
esac
emacsclient -c "$choice"
