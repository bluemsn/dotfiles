function tree --description 'A nice output of the `tree` command'
	command tree -CAFa -I 'CVS|*.*.package|.svn|.git|node_modules' --dirsfirst
end
