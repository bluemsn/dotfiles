# Startup commands
xbindkeys
setxkbmap -layout "latam" -variant "deadtilde"

# Startup programs
dropboxd &

# Xmonad
xsetroot -cursor_name left_ptr
exec xmonad

# fastwm
#exec fastwm

# Alopex
#exec alopex "/home/eduan/dotfiles/config/alopex/statusbar.sh"
#exec alopex "while true;do date +'%F %r';sleep 1;done"
