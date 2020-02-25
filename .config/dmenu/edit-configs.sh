#!/bin/sh
# Adapted from https://www.gitlab.com/dwt1/dotfiles/-/blob/master/.dmenu/dmenu-edit-configs.sh

declare options=(
"bashrc
bash_aliases
nvim/conf.vim
nvim/init.vim
nvim/keys.vim
nvim/plug.vim
nvim/ui.vim
openbox/rc.xml
openbox/autostart
picom
profile
qutebrowser
spacemacs
xmonad.hs
xmonad/kde-autostart.sh
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
    nvim/conf.vim)
        choice="$HOME/.config/nvim/conf.vim"
    ;;
    nvim/init.vim)
        choice="$HOME/.config/nvim/init.vim"
    ;;
    nvim/keys.vim)
        choice="$HOME/.config/nvim/keys.vim"
    ;;
    nvim/plug.vim)
        choice="$HOME/.config/nvim/plug.vim"
    ;;
    nvim/ui.vim)
        choice="$HOME/.config/nvim/ui.vim"
    ;;
    openbox/rc.xml)
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
    xmonad.hs)
        choice="$HOME/.xmonad/xmonad.hs"
    ;;
    xmonad/kde-autostart.sh)
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
