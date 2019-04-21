#!/bin/sh

# Locales
LC_CTYPE="fr_FR.UTF-8";	export LC_CTYPE
LC_ALL="fr_FR.UTF-8";	export LC_ALL
LANG="fr_FR.UTF-8";	export LANG

# XDG
XDG_CONFIG_HOME="$HOME/.config";	export XDG_CONFIG_HOME
XDG_CACHE_HOME="$HOME/.cache";		export XDG_CACHE_HOME
XDG_DATA_HOME="$HOME/.local/share";	export XDG_DATA_HOME

xrdb -m ~/.Xresources
setxkbmap -layout us -variant altgr-intl
xsetroot -cursor_name left_ptr

xrandr | grep "DP-2 connected" && \
  xrandr --output LVDS-1 --off \
         --output DP-2 --auto

xinput set-prop "/dev/wsmouse" "WS Pointer Wheel Emulation" 1
xinput set-prop "/dev/wsmouse" "WS Pointer Wheel Emulation Button" 2
xinput set-prop "/dev/wsmouse" "WS Pointer Wheel Emulation Axes" 6 7 4 5

exec bspwm
