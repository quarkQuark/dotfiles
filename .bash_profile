#
# ~/.bash_profile
#

[[ -f ~/.config/sh/envrc ]] && . ~/.config/sh/envrc
[[ -f ~/.bashrc ]] && . ~/.bashrc

if [ -e /home/jonathan/.nix-profile/etc/profile.d/nix.sh ]; then . /home/jonathan/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer
