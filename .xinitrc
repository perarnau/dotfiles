#!/bin/sh

# X11 config
xrdb -merge $HOME/.Xresources
xrdb -merge ~/.Xresources.dark
xsetroot -cursor_name left_ptr
xset b off
# disable printscreen
[[ -f ~/.Xmodmap ]] && xmodmap ~/.Xmodmap

# xrandr config at Argonne
SECOND_SCREEN=`xrandr | awk '/^DP.* connected/ { print $1 }'`
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
export CUPS_SERVER=localhost:631

eval `ssh-agent`
urxvtd -q -o -f &
sh ~/.fehbg &
xscreensaver &
nm-applet --sm-disable &
ibus-daemon --xim -d &
syndaemon -i 1 -d &
gnome-keyring-daemon &
start-pulseaudio-x11 &
dunst &
redshift &
exec xmonad
