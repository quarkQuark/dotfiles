#!/bin/sh

# Terminare alreadt running bar instances
killall -q polybar

# Wait until the processes have been shut down
while pgrep -u $UID -x polybar >/dev/null; do sleep 1; done

# Launch mybar
polybar mybar -r &
echo "Polybar loaded"
