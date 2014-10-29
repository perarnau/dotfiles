#!/bin/sh

# X11 config
xrdb -merge $HOME/.Xresources
xsetroot -cursor_name left_ptr
xset b off

# xrandr config at Argonne
SECOND_SCREEN=`xrandr | awk '/^DP. connected/ { print $1 }'`
if [[ "$SECOND_SCREEN" != "" ]]; then
	xrandr --output $SECOND_SCREEN --right-of eDP1
fi

export LANG="en_US.UTF-8"
export LC_ALL="en_US.UTF-8"
export LANGUAGE="en_US.UTF-8"
export LC_CTYPE="en_US.UTF-8"

export PATH=$PATH:$HOME/bin
export GTK_IM_MODULE=ibus
export XMODIFIERS=@im=ibus
export QT_IM_MODULE=ibus

sh ~/.fehbg &
xscreensaver &
nm-applet --sm-disable &
ibus-daemon --xim -d &
syndaemon -i 1 -d &
start-pulseaudio-x11 &
godark
exec xmonad
