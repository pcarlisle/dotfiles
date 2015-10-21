/usr/lib/gnome-settings-daemon/gnome-settings-daemon &
xfce4-power-manager &
xfce4-volumed-pulse &

[[ -f ~/.Xresources ]] && xrdb -merge ~/.Xresources

xsetroot -cursor_name left_ptr

xmodmap ~/.Xmodmap

eval `ssh-agent`

trayer --SetPartialStrut true --SetDockType true --edge top --align right --widthtype pixels --width 160 --tint 0x000000 --transparent true --alpha 0 --monitor 0 &

hsetroot -solid '#000000'
xcompmgr -n &

export _JAVA_AWT_WM_NONREPARENTING=1

exec /usr/bin/xmonad
