#!/bin/sh

sysctl hw.snd.default_unit=2

while true
do
    if xrandr | grep "DP-2 connected"
    then
        xrandr --output LVDS-1 --off \
               --output DP-2 --auto

        bspc wm --restart
        pkill -USR1 polybar

        break
    fi
done
