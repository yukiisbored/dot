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

exec bspwm
