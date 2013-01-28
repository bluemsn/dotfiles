" Set the window position to these coordinates
winpos 0 0

" Set the font
"set guifont=DejaVu\ Sans\ Mono:h12
set guifont=Monaco:h11

if ! $diff " Check if in diff mode
	" If not, do a normal sized window
	set columns=120 lines=40 " Set the width and height of window
else
	" If yes, then double that for diff mode
	set columns=240 lines=40 " Same here, duh!
endif

if has('gui_macvim')
	set fuoptions=maxvert,maxhorz " Full screen means FULL screen

	" Disable some stuff available in the GUI
	macmenu File.Save key=<nop>
	macmenu File.Save\ All key=<nop>
	macmenu File.Save\ As\.\.\. key=<nop>
else
	" Other GUIs, like Gvim, go here
endif

" Normal, visual and command-line normal (append) mode
set guicursor=n-v-c:block-Cursor-blinkon0
" Visual mode with 'selection' "exclusive" (same as 'v', if not specified)
set guicursor+=ve:ver35-Cursor
" Operator-pending mode
set guicursor+=o:hor50-Cursor-blinkwait175-blinkoff150-blinkon175
" Insert and command-line insert mode
set guicursor+=i-ci:ver20-Cursor
" Replace and command-line replace mode
set guicursor+=r-cr:hor20-Cursor
" Showmatch in insert mode
set guicursor+=sm:block-Cursor-blinkwait175-blinkoff150-blinkon175
