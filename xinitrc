# Startup commands
setxkbmap -layout "latam" -variant "deadtilde"
xsetroot -cursor_name left_ptr

# Startup programs
#dropboxd &
#pidgin &
#redshift-gtk &

# Xmonad
#trayer --edge top --align right --SetDockType true --SetPartialStrut true --expand true --widthtype pixel --width 100 --heighttype pixel --height 16 --transparent true --alpha 0 --tint 0x282828 &
#exec xmonad

# XFCE/LXDE
#exec startlxde

# bspwm
sxhkd &
exec bspwm

# Alopex
#exec alopex "/home/eduan/dotfiles/config/alopex/statusbar.sh"
#exec alopex "while true;do date +'%F %r';sleep 1;done"
