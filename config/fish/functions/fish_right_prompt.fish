function fish_right_prompt
	set_color normal
	date +"%H:%M"

	set_color red
	echo ' '
	echo -n "[$status]"
end
