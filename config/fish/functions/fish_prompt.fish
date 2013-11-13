function fish_prompt
	set_color green
	whoami | tr -d '\n'
	set_color normal
	echo -n '@'
	set_color green
	hostname | tr -d '\n'

	set_color blue
	echo -n ' '
	echo -n (prompt_pwd)

	set_color normal
	echo -n (__fish_git_prompt)

	set_color red
	echo -n ' $ '
	set_color normal
end
