if has('statusline')

" The default statusline is:
" set statusline=%<%f\ %h%m%r%=%-14.(%l,%c%V%)\ %P

" Current mode variable {{{
if (!exists('g:currentmode'))
	let g:currentmode={ 'n':'Normal', 'no':'N·Operator Pending', 'v':'Visual', 'V':'V·Line', '^V':'V·Block', 's':'Select', 'S':'S·Line', '^S':'S·Block', 'i':'Insert', 'R':'Replace', 'Rv':'V·Replace', 'c':'Command', 'cv':'Vim Ex', 'ce':'Ex', 'r':'Prompt', 'rm':'More', 'r?':'Confirm', '!':'Shell' }
endif
" }}}

" Colorize statusline {{{
function! Colorize()
	" Solarized && g:solarized_vars {{{
	if (g:colors_name == 'solarized' && exists('g:solarized_vars'))
		let s:using_solarized_vars=1
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

		return ''
	endif
	" }}}

	if (g:colors_name != 'solarized' || !exists('s:using_solarized_vars'))
		if (!exists('g:usefulstatusline_colorize_custom'))
			" Solarized definitions {{{

			" Set both gui and terminal color values in separate conditional statements
			" Due to possibility that CSApprox is running (though I suppose we could just
			" leave the hex values out entirely in that case and include only cterm colors)
			" We also check to see if user has set solarized (force use of the
			" neutral gray monotone palette component)
			if (has('gui_running') && g:solarized_degrade == 0)
				let s:vmode  = "gui"
				let s:base02 = "#073642"
				let s:base1  = "#93a1a1"
				let s:red    = "#dc322f"
				let s:green  = "#719e07" "experimental
			elseif (has('gui_running') && g:solarized_degrade == 1)
				" These colors are identical to the 256 color mode. They may be viewed
				" while in gui mode via "let g:solarized_degrade=1", though this is not
				" recommened and is for testing only.
				let s:vmode  = "gui"
				let s:base02 = "#262626"
				let s:base1  = "#8a8a8a"
				let s:red    = "#af0000"
				let s:green  = "#5f8700"
			elseif g:solarized_termcolors != 256 && &t_Co >= 16
				let s:vmode  = "cterm"
				let s:base02 = "0"
				let s:base1  = "14"
				let s:red    = "1"
				let s:green  = "2"
			elseif g:solarized_termcolors == 256
				let s:vmode  = "cterm"
				let s:base02 = "235"
				let s:base1  = "245"
				let s:red    = "124"
				let s:green  = "64"
			else
				let s:vmode  = "cterm"
				let s:bright = "* term=bold cterm=bold"
				let s:base02 = "Black"         " 0
				let s:base1  = "LightCyan"     " 6*
				let s:red    = "DarkRed"       " 1
				let s:green  = "DarkGreen"     " 2
			endif

			" Some Solarized colorscheme definitions
			let s:r = ',reverse'
			if (g:solarized_bold == 0 || &t_Co == 8 )
				let s:b  = ''
				let s:bb = ',bold'
			else
				let s:b  = ',bold'
				let s:bb = ''
			endif
			exe 'let s:fmt_none = " '.s:vmode.'=NONE'.' term=NONE'.'"'
			exe 'let s:fmt_revbb = " '.s:vmode.'=NONE'.s:r.s:bb.' term=NONE'.s:r.s:bb.'"'
			exe 'let s:bg_base02 = " '.s:vmode.'bg='.s:base02.'"'
			exe 'let s:fg_base1 = " '.s:vmode.'fg='.s:base1.'"'
			exe 'let s:fg_green = " '.s:vmode.'fg='.s:green.'"'
			exe 'let s:fg_red = " '.s:vmode.'fg='.s:red.'"'

			" }}}
		endif

		if (!exists('g:usefulstatusline_normal'))
			exe 'let g:usefulstatusline_normal="'.s:fmt_none.' '.s:vmode.'fg='.s:base1.' '.s:vmode.'bg='.s:base02.' '.s:fmt_revbb.'"'
		endif
		if (!exists('g:usefulstatusline_visual'))
			exe 'let g:usefulstatusline_visual="'.s:fmt_none.' '.s:vmode.'fg='.s:green.' '.s:vmode.'bg='.s:base02.' '.s:fmt_revbb.'"'
		endif
		if (!exists('g:usefulstatusline_insert'))
			exe 'let g:usefulstatusline_insert="'.s:fmt_none.' '.s:vmode.'fg='.s:red.' '.s:vmode.'bg='.s:base02.' '.s:fmt_revbb.'"'
		endif
		if (!exists('g:usefulstatusline_error'))
			let g:usefulstatusline_error=g:usefulstatusline_normal
		endif

		if(mode() =~# '\v(n|no)')
			exe 'hi! StatusLine '.g:usefulstatusline_normal
		elseif (mode() =~# '\v(v|V)' || g:currentmode[mode()] ==# 'V·Block')
			exe 'hi! StatusLine '.g:usefulstatusline_visual
		elseif (mode() ==# 'i')
			exe 'hi! StatusLine '.g:usefulstatusline_insert
		else
			exe 'hi! StatusLine '.g:usefulstatusline_error
		endif
	endif

	return ''
endfunction
" }}}

" File size {{{
function! FileSizePure(byte)
	" Check if we're gonna use the bit or the bytes approach (I call it that,
	" don't know if it's called that)
	" In this case, if the user wants to use the bytes approach:
	if (a:byte == 1)
		" Figure out how many bytes the buffer has
		let bytes = line2byte(line('$')+1)-1
		" If the bytes reach kilobytes
		if (bytes >= 1000)
			let kbytes = bytes / 1000
			let bytes_remainder = bytes % 1000
		endif
		" If kilobytes exist in the file, and the file reaches the size of
		" megabytes
		if (exists('kbytes') && kbytes >= 1000)
			let mbytes = kbytes / 1000
			let kbytes_remainder = kbytes % 1000
		endif

		" If the buffer is empty, output so
		if (bytes <= 0)
			return 'empty'
		endif

		" Check if the file is megabytes
		if (exists('mbytes'))
			" Output the file size in megabytes
			return printf('%u.%03u', mbytes, kbytes_remainder) . 'MB'
		" If megabytes don't exist, but kilobytes do
		elseif (exists('kbytes'))
			" Output the file size in kilobytes
			return printf('%u.%03u', kbytes, bytes_remainder) . 'kB'
		" If not even kylobytes exist
		else
			" Output the file size in bytes
			return bytes . 'B'
		endif

	" Go for the bits approach
	" I'm too lazy for more comments, so you can figure out the rest yourself
	elseif (a:byte == 0)
		let bytes = line2byte(line('$')+1)-1
		if (bytes >= 1024)
			let kibytes = bytes / 1024
			let bytes_remainder = bytes % 1024
		endif
		if (exists('kibytes') && kibytes >= 1024)
			let mebytes = kibytes / 1024
			let kibytes_remainder = kibytes % 1024
		endif

		if (bytes <= 0)
			return 'empty'
		endif

		if (exists('mebytes'))
			return printf('%u.%03u', mebytes, kibytes_remainder) . 'MiB'
		elseif (exists('kibytes'))
			return printf('%u.%03u', kibytes, bytes_remainder) . 'KiB'
		else
			return bytes . 'B'
		endif
	endif
endfunction
" }}}

" {{{
function! DateTimePure(format, time)
	if (a:format == 1)
		" Output date
		let s:format = '%m/%d/%y'
	elseif (a:format == 2)
		" Output time
		let s:format = '%T'
	elseif (a:format == 3)
		" Output date and time
		let s:format = '%m/%d/%y %T'
	endif

	if (a:time == 1)
		" Output last modified date and time of current buffer
		let s:time = getftime(expand("%"))
	elseif (a:time == 2)
		" Output current time
		let s:time = localtime()
	endif

	" Convert user options into a string
	let s:output = strftime(s:format, s:time)

	" Output results
	return s:output
endfunc

function! DateTime(format, time)
	" Output the same as the pure function, however it now puts it in between
	" brackets
	let s:output = '['.DateTimePure(a:format, a:time).']'
	return s:output
endfunc
" }}}

function! Statusline()
	let s:stl=''        " Clear statusline for when vimrc is loaded
	if exists('g:loaded_usefulstatusline')
		let s:stl.='%{Colorize()}'
		let s:stl.='[%{toupper(g:currentmode[mode()])}]'
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
	let s:stl.='%{&ft!=""?&ft.",":""}' " Filetype
	let s:stl.='%{&fenc!=""?&fenc.",":&enc.","}' " File encoding
	let s:stl.='%{(&bomb?"BOM,":"")}'  " BOM encoding
	let s:stl.='%{&ff!=""?&ff.",":""}' " Line ending?
	let s:stl.='%{FileSizePure(1)}'    " File size
	let s:stl.=']'    " Closing square bracket for file info
	let s:stl.=' '
	let s:stl.='%{DateTime(3, 1)}'
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
