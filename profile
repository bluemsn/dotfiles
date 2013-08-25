# Set the terminal
if $(which xclip); then
	export TERM='rxvt-unicode-256color'
elif [ -e /usr/share/terminfo/x/xterm-256color ]; then
	export TERM='xterm-256color'
else
	export TERM='xterm-color'
fi
