if has('statusline')

" The default statusline is:
" set statusline=%<%f\ %h%m%r%=%-14.(%l,%c%V%)\ %P

function! Statusline()
	let s:stl=''        " Clear statusline for when vimrc is loaded
	if exists('g:loaded_usefulstatusline')
		let s:stl.='%{usefulstatusline_colorize#Colorize()}'
		let s:stl.='[%{toupper(usefulstatusline_mode#CurrentModePure())}]'
	endif
	let s:stl.=' '      " Separator
	let s:stl.='[%02n]' " Buffer number of current buffer
	let s:stl.=' '      " Separator
	let s:stl.='{'
	let s:stl.='%(' " Start of item group
	let s:stl.='%M' " Show modified status of buffer
	let s:stl.='%R' " Show if file is read-only: RO
	let s:stl.='%W' " Show if buffer is a preview item?: PRV
	let s:stl.='%H' " Show if buffer is a help file: HLP
	let s:stl.='%)' " End of item group
	let s:stl.='}'
	let s:stl.=' '    " Separator
	let s:stl.='%<'   " Truncate from here on
	let s:stl.='"%t"' " Current buffer's file name
	let s:stl.=' '    " Separator
	let s:stl.='['    " Opening square bracket for file info
	let s:stl.='%{&ft!=""?&ft.",":""}'
	let s:stl.='%{&fenc!=""?&fenc.",":&enc.","}'
	let s:stl.='%{(&bomb?"BOM,":"")}'
	let s:stl.='%{&ff!=""?&ff.",":""}'
	if (exists('g:loaded_usefulstatusline'))
		let s:stl.='%{usefulstatusline_filesize#FileSizePure(1)}'
	endif
	let s:stl.=']'                   " Closing square bracket for file info
	if (exists('g:loaded_fugitive')) " If Fugitive is installed
		let s:stl.=' %{fugitive#statusline()}'
	endif

	let s:stl.='%='      " Right side of statusline
	let s:stl.=' '       " So there's a space between both sides
	let s:stl.='[%b:0x%B]' " Value of char under cursor, same in hexadecimal
	let s:stl.=' '       " Separator
	let s:stl.='[L%l/%L' " Cursor line and total of lines
	let s:stl.=':'       " Separator between line and column info
	let s:stl.='C%c-%v]' " Current column and virtual column
	let s:stl.=' '       " Separator
	let s:stl.='(%p%%)'  " Percentage through file in lines, as in <C-g>

	let &l:statusline=s:stl
endfunction

set statusline=%{Statusline()}

endif

" vim: set nowrap fdm={{{,}}}
