if has('statusline')

" The default statusline is:
" set statusline=%<%f\ %h%m%r%=%-14.(%l,%c%V%)\ %P

let &stl=''        " Clear statusline for when vimrc is loaded
let &stl.='%{usefulstatusline_colorize#Colorize()}'
let &stl.='[%{toupper(usefulstatusline_mode#CurrentModePure())}]'
let &stl.=' '      " Separator
let &stl.='[%02n]' " Buffer number of current buffer
let &stl.=' '      " Separator
let &stl.='{'
let &stl.='%('     " Start of item group
let &stl.='%M'     " Show modified status of buffer
let &stl.='%R'     " Show if file is read-only: RO
let &stl.='%W'     " Show if buffer is a preview item?: PRV
let &stl.='%H'     " Show if buffer is a help file: HLP
let &stl.='%)'    " End of item group
let &stl.='}'
let &stl.=' '      " Separator
let &stl.='%<'     " Truncate from here on
let &stl.='"%t"'   " Current buffer's file name
let &stl.=' '      " Separator
let &stl.='['      " Opening square bracket for file info
let &stl.='%{&ft!=""?&ft.",":""}'
let &stl.='%{&fenc!=""?&fenc.",":&enc.","}'
let &stl.='%{(&bomb?"BOM,":"")}'
let &stl.='%{&ff!=""?&ff.",":""}'
let &stl.='%{usefulstatusline_filesize#FileSizePure()}'
let &stl.=']'             " Closing square bracket for file info
if exists('*GitBranchInfoString')        " If GitBranchInfo exists
	let &stl.='%{GitBranchInfoString()}' " Buffer's Git info
endif

let &stl.='%='      " Right side of statusline
let &stl.=' '       " So there's a space between both sides
let &stl.='[%b:0x%B]' " Value of char under cursor and same in hexadecimal
let &stl.=' '       " Separator
let &stl.='[L%l/%L' " Cursor line and total of lines
let &stl.=':'       " Separator between line and column info
let &stl.='C%c-%v]' " Current column and virtual column
let &stl.=' '       " Separator
let &stl.='(%p%%)'  " Percentage through file in lines, as in <C-g>

endif

" vim: set nowrap fdm={{{,}}}
