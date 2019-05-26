#!/bin/sh

sysctl hw.snd.default_unit=0

xrandr --output LVDS-1 --auto \
       --output DP-2 --off

bspc wm --restart
pkill -USR1 polybar
