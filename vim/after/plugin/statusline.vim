if has('multi_statusline') "{{{1
	" If you don't know what this means:
	" https://gist.github.com/3158492

	" The amount of lines the statusline is allowed to have
	set statuslineheight=2

	let &stl=''		   " Clear statusline for when vimrc is loaded
	let &stl.='[%02n]' " Buffer number of current buffer
	let &stl.=' '	   " Separator
	let &stl.='{'	   " Opening curly bracket, for item group
	let &stl.='%('	   " Start of item group
	let &stl.='%M'	   " Show modified status of buffer: -, +, 
	let &stl.='%R'	   " Show if file is read-only: RO
	let &stl.='%W'	   " Show if buffer is a preview item?: PRV
	let &stl.='%H'	   " Show if buffer is a help file: HLP
	let &stl.='%)'	   " End of item group
	let &stl.='}'	   " Closing curly bracket, for item group
	let &stl.=' '	   " Separator
	let &stl.='[%{&ft!=""?&ft.", ":""}'  " Buffer's file type
	let &stl.='%{&fenc!=""?&fenc:&enc}'  " Buffer's encoding
	let &stl.='%{&ff!=""?", ".&ff:""}]'  " Buffer's file type
	if exists('*GitBranchInfoString')
		let &stl.='%{GitBranchInfoString()}' " Buffer's Git info
	endif
	let &stl.='%='	   " Right side of first statusline
	let &stl.=' '	   " Separator
	let &stl.='%<'	   " Truncate from here on
	let &stl.='%F'	   " Full path to file
	let &stl.='%@'	   " Go to next line of statusline

	let &stl.='[%b'    " Value of character under cursor
	let &stl.=':'	   " Separator between '%b' and '%B'
	let &stl.='0x%B]'  " As above, in hexadecimal
	let &stl.=' '	   " Separator
	let &stl.='[L%l'   " Cursor's current line
	let &stl.='/'	   " Separator for current line and total of lines
	let &stl.='%L'	   " Total of lines
	let &stl.=':'	   " Separator between line and column info
	let &stl.='C%c'    " Current column
	let &stl.='-'	   " Separator between column and virtual column
	let &stl.='%v]'    " Current virtual column
	let &stl.=' '	   " Separator
	let &stl.='(%p%%)' " Percentage through file in lines, as in <c-g>
	let &stl.='%='	   " Right side of second statusline
	let &stl.=' '	   " Separator
	let &stl.='%f'

elseif has('statusline') "{{{1
	let &stl=''		   " Clear statusline for when vimrc is loaded
	let &stl.='[%02n]' " Buffer number of current buffer
	let &stl.=' '	   " Separator
	let &stl.='{'	   " Opening curly bracket, for item group
	let &stl.='%('	   " Start of item group
	let &stl.='%M'	   " Show modified status of buffer: -, +, 
	let &stl.='%R'	   " Show if file is read-only: RO
	let &stl.='%W'	   " Show if buffer is a preview item?: PRV
	let &stl.='%H'	   " Show if buffer is a help file: HLP
	let &stl.='%)'	   " End of item group
	let &stl.='}'	   " Closing curly bracket, for item group
	let &stl.=' '	   " Separator
	let &stl.='%<'	   " Truncate from here on
	let &stl.='%F'	   " Full path to file
	let &stl.=' '	   " Separator
	let &stl.='[%{&ft!=""?&ft.", ":""}'  " Buffer's file type
	let &stl.='%{&fenc!=""?&fenc:&enc}'  " Buffer's encoding
	let &stl.='%{&ff!=""?", ".&ff:""}]'  " Buffer's file type
	if exists('*GitBranchInfoString')
		let &stl.='%{GitBranchInfoString()}' " Buffer's Git info
	endif

	let &stl.='%='	   " Right side of statusline
	let &stl.=' '	   " So there's a space between both sides
	let &stl.='[%b'    " Value of character under cursor
	let &stl.=':'	   " Separator between '%b' and '%B'
	let &stl.='0x%B]'  " As above, in hexadecimal
	let &stl.=' '	   " Separator
	let &stl.='[L%l'   " Cursor's current line
	let &stl.='/'	   " Separator for current line and total of lines
	let &stl.='%L'	   " Total of lines
	let &stl.=':C'	   " Separator between line and column info
	let &stl.='%c'     " Current column
	let &stl.='-'	   " Separator between '%c' and '%v'
	let &stl.='%v]'    " Current virtual column
	let &stl.=' '	   " Separator
	let &stl.='(%p%%)' " Percentage through file in lines, as in <c-g>
endif "}}}1

" vim: set nowrap fdm={{{,}}}
