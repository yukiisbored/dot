#!/bin/sh

compton --config ~/.config/compton/compton.conf -b &
xfce4-power-manager &
xscreensaver -no-splash &
nitrogen --restore &
$HOME/.screenlayout/muh_screens.sh &
dunst &

exec ck-launch-session dbus-launch cwm
