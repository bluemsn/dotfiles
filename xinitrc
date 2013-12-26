# Startup commands
xbindkeys
setxkbmap -layout "latam" -variant "deadtilde"

# Startup programs
#dropboxd &
pidgin &

# Xmonad
trayer --edge top --align right --SetDockType true --SetPartialStrut true --expand true --width 5 --height 15 --transparent true --tint 0x000000 &
xsetroot -cursor_name left_ptr
exec xmonad

# fastwm
#exec fastwm

# Alopex
#exec alopex "/home/eduan/dotfiles/config/alopex/statusbar.sh"
#exec alopex "while true;do date +'%F %r';sleep 1;done"
