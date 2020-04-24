# If not running interactively, don't do anything
[[ $- != *i* ]] && return

export PATH=$PATH:~/.local/bin

# Avoid duplicate prompt
PROMPT_COMMAND=

# Prompt colour escape sequences
PROMPT_BLACK="\[\033[30m\]"
PROMPT_RED="\[\033[31m\]"
PROMPT_GREEN="\[\033[32m\]"
PROMPT_YELLOW="\[\033[33m\]"
PROMPT_BLUE="\[\033[34m\]"
PROMPT_PURPLE="\[\033[35m\]"
PROMPT_CYAN="\[\033[36m\]"
PROMPT_WHITE="\[\033[37m\]"
PROMPT_UNCOLOUR="\[\033[m\]"

# Print colour-coded battery percentage
battery_prompt () {
    BATTERY_POWER=`cat /sys/class/power_supply/BAT0/capacity`
    [ $BATTERY_POWER -ge 60 ] \
        && echo "$PROMPT_GREEN$BATTERY_POWER"
    [ $BATTERY_POWER -lt 60 ] && [ $BATTERY_POWER -ge 20 ] \
        && echo "$PROMPT_YELLOW$BATTERY_POWER"
    [ $BATTERY_POWER -lt 20 ] \
        && echo "$PROMPT_RED$BATTERY_POWER"
}

# Prompt template: <battery[time]pwd$ >
PS1="\
`battery_prompt`\
$PROMPT_CYAN[\T]\
$PROMPT_BLUE\w\
$PROMPT_WHITE\$\
$PROMPT_UNCOLOUR "

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

# Check and update neofetch cache
NEOFETCH_CACHE=$XDG_CACHE_HOME/neofetch.txt
command -v neofetch >/dev/null \
    && ! find $NEOFETCH_CACHE -mtime -1 >/dev/null \
    && neofetch > $NEOFETCH_CACHE

# Print out neofetch cache
[ -f $NEOFETCH_CACHE ] \
    && cat $NEOFETCH_CACHE \
    || echo "neofetch not found"

[ -f ~/.bash_aliases ] && source ~/.bash_aliases

# Exa configuration and colours (ls alternative)
EXA_CONFIG_FILE=$XDG_CONFIG_HOME/exa/exa-config.sh
[ -f $EXA_CONFIG_FILE ] && source $EXA_CONFIG_FILE
