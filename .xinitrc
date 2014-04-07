#!/bin/sh

# Define Xresources
userresources=$HOME/.Xresources

# Merge what is available
if [ -f "$userresources" ]; then
	xrdb -merge "$userresources"
fi

# set pointer
xsetroot -cursor_name left_ptr

sh ~/.fehbg &
xscreensaver &

export LANG="en_US.UTF-8"
export LC_ALL="en_US.UTF-8"
export LANGUAGE="en_US.UTF-8"
export LC_CTYPE="en_US.UTF-8"

exec ck-launch-session dbus-launch xmonad
