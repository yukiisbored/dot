#!/bin/sh

# Add local PATH
PATH="$HOME/bin:$PATH"; export PATH

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

[ "$(sysctl -n dev.acpi_dock.0.status)" -eq "1" ] && ~/.config/dock-trigger/docked.sh

xinput set-prop "/dev/wsmouse" "WS Pointer Wheel Emulation" 1
xinput set-prop "/dev/wsmouse" "WS Pointer Wheel Emulation Button" 2
xinput set-prop "/dev/wsmouse" "WS Pointer Wheel Emulation Axes" 6 7 4 5

xscreensaver -no-splash &
compton --config /home/yuki/.config/compton/compton.conf -b
$HOME/.local/bin/dock-trigger &
nextcloud --background &

exec awesome
