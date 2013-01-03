if has('statusline')

" Define all the different modes
let g:currentmode={
	\ 'n'  : 'Normal',
	\ 'no' : 'N·Operator Pending',
	\ 'v'  : 'Visual',
	\ 'V'  : 'V·Line',
	\ '' : 'V·Block',
	\ 's'  : 'Select',
	\ 'S'  : 'S·Line',
	\ '' : 'S·Block',
	\ 'i'  : 'Insert',
	\ 'R'  : 'Replace',
	\ 'Rv' : 'V·Replace',
	\ 'c'  : 'Command',
	\ 'cv' : 'Vim Ex',
	\ 'ce' : 'Ex',
	\ 'r'  : 'Prompt',
	\ 'rm' : 'More',
	\ 'r?' : 'Confirm',
	\ '!'  : 'Shell',
	\}

function! ChangeStatuslineColor() "{{{
	if (g:colors_name == 'solarized' && exists('g:solarized_vars'))
		let s:vars=g:solarized_vars

		if (mode() =~# '\v(n|no)')
			exe 'hi! StatusLine '.s:vars['fmt_none'].s:vars['fg_base1'].s:vars['bg_base02'].s:vars['fmt_revbb']
		elseif (mode() =~# '\v(v|V)' || g:currentmode[mode()] ==# 'V·Block')
			exe 'hi! StatusLine'.s:vars['fmt_none'].s:vars['fg_green'].s:vars['bg_base02'].s:vars['fmt_revbb']
		elseif (mode() ==# 'i')
			exe 'hi! StatusLine'.s:vars['fmt_none'].s:vars['fg_red'].s:vars['bg_base02'].s:vars['fmt_revbb']
		else
			exe 'hi! StatusLine '.s:vars['fmt_none'].s:vars['fg_base1'].s:vars['bg_base02'].s:vars['fmt_revbb']
		endif
	endif

	return ''
endfunction "}}}
function! FileSize() "{{{
	" Set a basic variable that equals the file's size in bytes
	let bytes = getfsize(expand('%:p'))

	" Set a variable that outputs KBs, only when there's KBs in file size
	if (bytes >= 1024)
		let kbytes = bytes / 1024
	endif

	" Set a variable that outputs MBs, only when there's MBs in file size
	if (exists('kbytes') && kbytes >= 1000)
		let mbytes = kbytes / 1000
	endif

	" Return 'empty' if the file has no size
	if (bytes <= 0)
		return 'empty'
	endif

	if (exists('mbytes'))
		" Output MBs if they exist
		return mbytes . 'MB'
	elseif (exists('kbytes'))
		" Output KBs, if MBs don't exist
		return kbytes . 'KB'
	else
		" Otherwise return bytes
		return bytes . 'B'
	endif
endfunction "}}}

" Statusline {{{
" The default statusline is:
" set statusline=%<%f\ %h%m%r%=%-14.(%l,%c%V%)\ %P

let &stl=''        " Clear statusline for when vimrc is loaded
let &stl.='%{ChangeStatuslineColor()}'
let &stl.='[%{toupper(g:currentmode[mode()])}]'
let &stl.=' '      " Separator
let &stl.='[%02n]' " Buffer number of current buffer
let &stl.=' '      " Separator
let &stl.='{'      " Opening curly bracket, for item group
let &stl.='%('     " Start of item group
let &stl.='%M'     " Show modified status of buffer
let &stl.='%R'     " Show if file is read-only: RO
let &stl.='%W'     " Show if buffer is a preview item?: PRV
let &stl.='%H'     " Show if buffer is a help file: HLP
let &stl.='%)'     " End of item group
let &stl.='}'      " Closing curly bracket, for item group
let &stl.=' '      " Separator
let &stl.='%<'     " Truncate from here on
let &stl.='"%t"'   " Current buffer's file name
let &stl.=' '      " Separator
let &stl.='['      " Opening square bracket for file info
let &stl.='%{&ft!=""?&ft.",":""}'
let &stl.='%{&fenc!=""?&fenc.",":&enc.","}'
let &stl.='%{&ff!=""?&ff.",":""}'
let &stl.='%{FileSize()}' " Output buffer's file size
let &stl.=']'             " Closing square bracket for file info
if exists('*GitBranchInfoString')        " If GitBranchInfo exists
	let &stl.='%{GitBranchInfoString()}' " Buffer's Git info
endif

let &stl.='%='     " Right side of statusline
let &stl.=' '      " So there's a space between both sides
let &stl.='[%b'    " Value of character under cursor
let &stl.=':'      " Separator between '%b' and '%B'
let &stl.='0x%B]'  " As above, in hexadecimal
let &stl.=' '      " Separator
let &stl.='[L%l'   " Cursor's current line
let &stl.='/'      " Separator for current line and total of lines
let &stl.='%L'     " Total of lines
let &stl.=':'      " Separator between line and column info
let &stl.='C%c'    " Current column
let &stl.='-'      " Separator between '%c' and '%v'
let &stl.='%v]'    " Current virtual column
let &stl.=' '      " Separator
let &stl.='(%p%%)' " Percentage through file in lines, as in <c-g>
" }}}

endif " if has('statusline')

" vim: set nowrap fdm={{{,}}}
