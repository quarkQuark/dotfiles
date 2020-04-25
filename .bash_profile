#
# ~/.bash_profile
#

[[ -f ~/.config/sh/env ]] && . ~/.config/sh/env
[[ -f ~/.bashrc ]] && . ~/.bashrc

if [ -e /home/jonathan/.nix-profile/etc/profile.d/nix.sh ]; then . /home/jonathan/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer
