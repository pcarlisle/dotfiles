userresources=$HOME/.Xresources
usermodmap=$HOME/.Xmodmap
sysresources=/etc/X11/xinit/.Xresources
sysmodmap=/etc/X11/xinit/.Xmodmap

# merge in defaults and keymaps
if [ -f $sysresources ]; then
    xrdb -merge $sysresources
fi

if [ -f $sysmodmap ]; then
    xmodmap $sysmodmap
fi

if [ -f "$userresources" ]; then
    xrdb -merge "$userresources"
fi

if [ -f "$usermodmap" ]; then
    xmodmap "$usermodmap"
fi

# start some nice programs

if [ -d /etc/X11/xinit/xinitrc.d ] ; then
 for f in /etc/X11/xinit/xinitrc.d/?*.sh ; do
  [ -x "$f" ] && . "$f"
 done
 unset f
fi

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
