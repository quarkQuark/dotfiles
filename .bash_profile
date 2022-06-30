# See https://blog.flowblok.id.au/2013-02/shell-startup-scripts.html

# Run bash/env first and prevent it from being run later
# (We need to use $BASH_ENV later for non-login non-interactive shells)
# Not exported becaues a non-login non-interactive shell may be a child
. ~/.config/bash/env
BASH_ENV=

# Run bash/login
. ~/.config/bash/login

# Run bash/interactive if this is an interactive shell
if [ "$PS1" ]; then
    . ~/.config/bash/interactive
fi
