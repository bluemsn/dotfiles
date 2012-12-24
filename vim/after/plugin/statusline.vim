if has('statusline')
" Find out current buffer's size and output it.
" https://github.com/blueyed/dotfiles/blob/master/vimrc#L384
function! FileSize() "{{{
	let bytes = getfsize(expand('%:p'))
	if (bytes >= 1024)
		let kbytes = bytes / 1024
	endif
	if (exists('kbytes') && kbytes >= 1000)
		let mbytes = kbytes / 1000
	endif

	if bytes <= 0
		return 'null'
	endif

	if (exists('mbytes'))
		return mbytes . 'MB'
	elseif (exists('kbytes'))
		return kbytes . 'KB'
	else
		return bytes . 'B'
	endif
endfunction "}}}

" Shorten a given filename by truncating path segments.
" https://github.com/blueyed/dotfiles/blob/master/vimrc#L396
function! ShortenFilename(bufname, maxlen) "{{{
	if getbufvar(bufnr(a:bufname), '&filetype') == 'help'
		return fnamemodify(a:bufname, ':t')
	endif

	let maxlen_of_parts = 7 " including slash/dot
	let maxlen_of_subparts = 5 " split at dot/hypen/underscore; including split

	let s:PS = exists('+shellslash') ? (&shellslash ? '/' : '\') : "/"
	let parts = split(a:bufname, '\ze['.escape(s:PS, '\').']')
	let i = 0
	let n = len(parts)
	let wholepath = '' " used for symlink check
	while i < n
		let wholepath .= parts[i]
		" Shorten part, if necessary:
		if i<n-1 && len(a:bufname) > a:maxlen && len(parts[i]) > maxlen_of_parts
		" Let's see if there are dots or hyphens to truncate at, e.g.
		" 'vim-pkg-debian' => 'v-p-d…'
		let w = split(parts[i], '\ze[._-]')
		if len(w) > 1
			let parts[i] = ''
			for j in w
			if len(j) > maxlen_of_subparts-1
				let parts[i] .= j[0:maxlen_of_subparts-2]."…"
			else
				let parts[i] .= j
			endif
			endfor
		else
			let parts[i] = parts[i][0:maxlen_of_parts-2].'…'
		endif
		endif
		" add indicator if this part of the filename is a symlink
		if getftype(wholepath) == 'link'
		if parts[i][0] == s:PS
			let parts[i] = parts[i][0] . '↬ ' . parts[i][1:]
		else
			let parts[i] = '↬ ' . parts[i]
		endif
		endif
		let i += 1
	endwhile
	let r = join(parts, '')
	return r
endfunction "}}}

" Output the current mode.
function! CurrentMode() "{{{
	let mode=mode()

	if (mode ==# 'v')
		let mode='Visual'
	elseif (mode ==# 'V')
		let mode='V-Line'
	elseif (mode ==# '^V')
		let mode='V-Block'
	elseif (mode ==# 's')
		let mode='Select'
	elseif (mode ==# 'S')
		let mode='S-Line'
	elseif (mode ==# '^S')
		let mode='S-Block'
	elseif (mode ==# 'i')
		let mode='Insert'
	elseif (mode ==# 'R')
		let mode='Replace'
	elseif (mode ==# 'Rv')
		let mode='V-Replace'
	elseif (mode ==# 'c')
		let mode='Command'
	elseif (mode ==# 'cv')
		let mode='Vim Ex'
	elseif (mode ==# 'ce')
		let mode='Ex'
	elseif (mode ==# 'r')
		let mode='Prompt'
	elseif (mode ==# 'rm')
		let mode='More'
	elseif (mode ==# 'r?')
		let mode='Confirm'
	elseif (mode ==# '!')
		let mode='Shell'
	else
		let mode='Normal'
	endif

	return mode
endfunc "}}}

" Output current file's info like size format encoding etc.
function! FileInfo() "{{{
	let fileinfo_output=''

	if (&ft != '')
		let fileinfo_output+='%{&ft!=""?&ft.",":""}'
	endif

	if (&fenc != '' || &enc != '')
		let fileinfo_output+='%{&fenc!=""?&fenc.",":&enc.","}'
	endif

	if (&ff != 'unix')
		let fileinfo_output+='%{&ff=="unix"?"":&ff.","}'
	endif

	if (exists('*FileSize'))
		let fileinfo_output+='%{FileSize()}'
	endif

	return fileinfo_output
endfunc "}}}

let &stl=''        " Clear statusline for when vimrc is loaded
let &stl.='[%{CurrentMode()}]'
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
let &stl.='%t'     " Current buffer's file name
let &stl.=' '      " Separator
let &stl.='['      " Opening square bracket for file info
if (exists('*FileInfo'))
	let &stl.='%{FileInfo()}'
endif
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
endif

" vim: set nowrap fdm={{{,}}}
