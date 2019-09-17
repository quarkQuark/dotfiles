# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# For xmonad and stack:
export PATH=$PATH:~/.local/bin

PROMPT_COMMAND=  # Avoid duplicate prompt
# 'user@host:path$ '
PS1="\033[36m\u\
\033[m@\
\033[36m\h\
\033[m:\
\033[34m\w\
\033[37m\$\
\033[m "

# Set up bash history
HISTCONTROL=ignoredups:ignorespace # Don't put duplicate lines in history
shopt -s histappend # Append rather than overwriting
HISTSIZE=1000
HISTFILESIZE=2000
export HISTTIMEFORMAT="%d/%m/%y %T "

# Check window size after each command
shopt -s checkwinsize

# Make less more friendly for non-plain text files
#[ -x /usr/bin/lesspipe.sh ] && eval "$(SHELL=/bin/sh lesspipe)"
LESSOPEN="|lesspipe.sh %s"; export LESSOPEN

. ~/.bash_aliases

## Make ls show correct colours
#DIR_COLORS="$HOME/.dircolors"
#test -r $d && eval "$(dircolors ~/.dircolors)"

neofetch

powerline-daemon -q
POWERLINE_BASH_CONTINUATION=1
POWERLINE_BASH_SELECT=1
. /usr/share/powerline/bindings/bash/powerline.sh
