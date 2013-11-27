function tree --description 'A nice output of the `tree` command'
	command tree -CAFa -I 'CVS|*.*.package|.svn|.git' --dirsfirst
end
