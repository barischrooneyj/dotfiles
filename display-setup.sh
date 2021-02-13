#!/bin/sh
set -x

until [ xrandr ]; do sleep 0.1; done
/usr/share/xrandr.sh
# feh --bg-fill dotfiles/background.jpg
