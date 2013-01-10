" Disable saving using 'Cmd+s'
if has('gui_macvim')
	macmenu File.Save key=<nop>
	macmenu File.Save\ All key=<nop>
	macmenu File.Save\ As\.\.\. key=<nop>
endif

" Fix spell check highlighting
highlight SpellBad term=underline gui=undercurl guisp=Red

" Set the window position to these coordinates
winpos 0 0

" Set the font
set guifont=DejaVu\ Sans\ Mono:h12

if ! $diff " Check if in diff mode
	" If not, do a normal sized window
	set columns=120 lines=40 " Set the width and height of window
else
	" If yes, then double that for diff mode
	set columns=240 lines=40 " Same here, duh!
endif

if has('gui_macvim')
	set fuoptions=maxvert,maxhorz " Full screen means FULL screen
else
	" Other GUIs, like Gvim, go here
endif
