#!/usr/bin/sh

# Alt Gr sends 'mod1' (same as Alt)
#xmodmap -e 'remove mod5 = ISO_Level3_Shift'
#xmodmap -e 'add mod1 = ISO_Level3_Shift'
setxkbmap -option lv3:ralt_alt

# Caps Lock sends 'mod3'
setxkbmap -option caps:hyper       # The caps key is now seen by the system as Hyper
xmodmap -e 'remove mod4 = Hyper_L' # Hyper used to do the same as Super
xmodmap -e 'add mod3 = Hyper_L'    # But now Hyper does its own thing!
# Hyper (Caps Lock) on release sends C-esc
pkill xcape # Avoid typing multiple spaces on one keypress
xcape -e 'Hyper_L=Control_L|Escape'

# S on release sends C-M-S-F1
#xcape -e 'Shift_L=Super_L|Control_L|Alt_L|Shift_L|F1'
#xcape -e 'Shift_R=Super_R|Control_R|Alt_R|Shift_R|F1'

# Holding Space (keycode 65) sends Super
xmodmap -e "keycode 65 = Super_L" # Space (65) generates Super
# Map space to an unused keycode to keep it accessible to xcape
xmodmap -e "keycode any = space"
# Tapping space works properly
xcape -e "Super_L=space" # Space works normally when tapped
