# If not running interactively, don't do anything
[[ $- != *i* ]] && return

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
HISTIGNORE="$HISTIGNORE:jrnl *"

# Check window size after each command
shopt -s checkwinsize

# Make less more friendly for non-plain text files
#[ -x /usr/bin/lesspipe.sh ] && eval "$(SHELL=/bin/sh lesspipe)"
LESSOPEN="|lesspipe.sh %s"; export LESSOPEN

if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

# Make ls show correct colours
DIR_COLORS="$HOME/.dircolors"
test -r $d && eval "$(dircolors ~/.dircolors)"

export TERM="screen-256color"

source /usr/share/autojump/autojump.bash

neofetch
