#!/bin/sh

# Add local PATH
export PATH="$HOME/.local/bin:$PATH"

# Locales
export LC_CTYPE="en_US.UTF-8"
export LC_ALL="en_US.UTF-8"
export LANG="en_US.UTF-8"

# XDG
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_CACHE_HOME="$HOME/.cache"
export XDG_DATA_HOME="$HOME/.local/share"

xrdb -m ~/.Xresources
setxkbmap -layout us -variant altgr-intl
xsetroot -cursor_name left_ptr
nitrogen --restore
xidle &
xinput set-prop "/dev/wsmouse" "WS Pointer Wheel Emulation" 1
xinput set-prop "/dev/wsmouse" "WS Pointer Wheel Emulation Button" 2
xinput set-prop "/dev/wsmouse" "WS Pointer Wheel Emulation Axes" 6 7 4 5
gnome-keyring-daemon --replace --daemonize
flameshot &
picom --config /dev/null -b
nextcloud &
/usr/local/lib/xfce4/notifyd/xfce4-notifyd &

exec dbus-launch --exit-with-session -- emacs -f exwm-enable
