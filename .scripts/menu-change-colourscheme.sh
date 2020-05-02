#!/usr/bin/sh

# Use the command line argument to define a menu program
MENU_CMD=$1

# My script to change colourschemes
cs="sh $HOME/.scripts/colourscheme.sh"

# List the available colourschemes
options=`$cs list`

# Choose one, using the user-defined menu program
choice=`echo "$options" | $MENU_CMD "Change colourscheme to: "`

# Change the colourscheme to the one chosen
$cs $choice
