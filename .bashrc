# If not running interactively, don't do anything
[[ $- != *i* ]] && return

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

command -v neofetch >/dev/null && neofetch || echo "neofetch not found"

[ -f ~/.bash_aliases ] && source ~/.bash_aliases

# Powerline
powerline-daemon -q
POWERLINE_BASH_CONTINUATION=1
POWERLINE_BASH_SELECT=1
source /usr/share/powerline/bindings/bash/powerline.sh

# Exa configuration and colours (ls alternative)
EXA_CONFIG_FILE=$XDG_CONFIG_HOME/exa/exa-config.sh
command -v exa >/dev/null && [ -f $EXA_CONFIG_FILE ] && source $EXA_CONFIG_FILE
