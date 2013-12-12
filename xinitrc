# Startup programs/commands
setxkbmap -layout "latam" -variant "deadtilde"
dropboxd &

# WM
#exec openbox-session
#exec enlightenment_start
exec alopex "/home/eduan/dotfiles/config/alopex/statusbar.sh"
#exec alopex "while true;do date +'%F %r';sleep 1;done"
