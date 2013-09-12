" ==============
" Basic settings
" ==============

set nocompatible
set hidden
set modeline
set viminfo+=!

" http://twitter.com/mbadran/status/111011179907915776
set clipboard+=unnamed
set clipboard+=unnamedplus

" http://crumbtrail.chesmart.in/post/5024677985/man-vim-dude
runtime! ftplugin/man.vim

" https://github.com/tpope/vim-sensible
if (!exists('g:loaded_matchit') && findfile('plugin/matchit.vim', &rtp) ==# '')
  runtime! macros/matchit.vim
endif

" ===========================
" Pathogen and plugin settings
" ===========================

" Execute Pathogen
execute pathogen#infect()
filetype plugin indent on

" Vitality.vim {{{
let g:vitality_fix_cursor=0
" }}}

" ===================
" Search and matching
" ===================

set iskeyword+=-
set wrapscan
set ignorecase
set smartcase
set novisualbell
set noerrorbells
set incsearch
set noshowmatch
set matchpairs=(:),{:},[:],':',":"
set showcmd
set magic
set formatoptions=vt
set nojoinspaces

if (&t_Co > 2 || has('gui_running'))
	set hls
endif

if (has('win32') || has('win64'))
	" This is for Windows/cygwin and to add -H.
	" '$*' is not passed to the shell, but used by Vim.
	set grepprg=grep\ -nH\ $*\ /dev/null
endif

" ===========
" Indentation
" ===========

set backspace=indent,eol,start
set noexpandtab
set shiftround
set smartindent
set autoindent

let s:tabwidth=4
exec 'set tabstop='    .s:tabwidth
exec 'set softtabstop='.s:tabwidth
exec 'set shiftwidth=' .s:tabwidth

" ================
" Command settings
" ================

set cpoptions+=$

" =======
" History
" =======

set history=1000

" =====================
" Backup and undo files
" =====================

set backup
set swapfile
set nowritebackup
set backupdir=~/.vim/tmpdir
set directory=~/.vim/tmpdir
if (!isdirectory(expand(&backupdir)))
	call mkdir(expand(&backupdir), 'p')
endif

if (has('persistent_undo'))
	set undofile
	set undodir=~/.vim/undodir
	if (!isdirectory(expand(&undodir)))
		call mkdir(expand(&undodir), 'p')
	endif
endif

" =====
" Folds
" =====

set foldenable
set foldmethod=marker
set foldlevelstart=0
set foldopen=block,insert,jump,mark,percent,quickfix,search,tag,undo

" ========================
" Mappings and re-mappings
" ========================

set noesckeys
set timeout

" https://powerline.readthedocs.org/en/latest/tipstricks.html#vim
set ttimeoutlen=2000
augroup FastEscape
	autocmd!
	au InsertEnter * set timeoutlen=0
	au InsertLeave * set timeoutlen=2000
augroup END

no <F1> <esc>
let mapleader=','
let maplocalLeader = '\\'

nn <C-e> ,
vn <C-e> ,

" Completely disable the use of the arrow keys in command and visual modes
no <up> <NOP>
no <down> <NOP>
no <left> <NOP>
no <right> <NOP>
ino <up> <NOP>
ino <down> <NOP>
ino <left> <NOP>
ino <right> <NOP>

" Fix moving line by line in a paragraph when soft wrap is on
nn j gj
nn k gk
vn j gj
vn k gk

" Smart way to move between windows
nn <C-j> <C-w>j
nn <C-k> <C-w>k
nn <C-h> <C-w>h
nn <C-l> <C-w>l

" http://twitter.com/dmedvinsky/status/109304047206547456
nn <silent> <leader>hh :setl hls<CR>:let @/="\\<<C-r><C-w>\\>"<CR>

" Preserve indentation while pasting text from the OS X clipboard
no <leader>p :set paste<CR>:put  *<CR>:set nopaste<CR>

" Better help tags navigation (IMO)
nn <C-S-Right> <C-]>
nn <C-S-Left>  <C-t>

" Indent in visual and select mode automatically re-selects
vn > >gv
vn < <gv

" Fix the '&' command in normal and visual modes
" https://github.com/nelstrom/dotfiles/blob/d245b5cf67/vimrc#L99-L101
nn & :&&<CR>
xn & :&&<CR>

" https://github.com/blueyed/dotfiles/blob/4407ba7905/vimrc#L1129-L1131
nn Y y$
xn Y y$

" <C-e> and <C-y> scroll the viewport a single line, bump this up a bit
nn <C-e> 3<C-e>
nn <C-y> 3<C-y>

" Allow change of theme from light to dark and vice-versa, with a hotkey
call togglebg#map('<F5>')


nn ñ :w<CR>
nn <S-ñ> :wq!<CR>

nn <leader>b :NERDTreeToggle<CR>
nn <leader>B :EasyBuffer<CR>

" ==============
" Screen drawing
" ==============

set cmdheight=2
set shellslash
set showmode
set showcmd
set report=0
set list
set listchars=tab:\|\ ,eol:$,trail:_
set nowrap
set linebreak
set number
set relativenumber
set colorcolumn=79
set shortmess=astI
set ttyfast
set linespace=0
set lazyredraw
set laststatus=2
set scrolloff=30
set sidescrolloff=10
set sidescroll=1
set virtualedit=all
set mousehide
set mouse=
set ruler
set statusline=%n:[%t]\ %m%r%w%<%=[L%l/%L\ C%c-%v]\ (%p%%)

" =======================
" Window/split management
" =======================

set title
set fillchars=stl:\ ,stlnc:\ ,vert:\|,fold:-,diff:-
set autowrite
set autoread
set tabpagemax=1
set showtabline=0
set switchbuf=useopen,usetab

" ===============
" Syntax coloring
" ===============

if (&t_Co > 2 || has('gui_running'))
	syntax on
endif
if (!has('gui_running'))
	let g:solarized_termtrans=1
	if (&t_Co >= 256 || $TERM == 'xterm-256color')
	else
		let g:solarized_termcolors=16
	endif
endif

let g:solarized_contrast='normal'
let g:solarized_visibility='normal'
let g:solarized_hitrail=1
let g:solarized_menu=0
let g:solarized_underline=1
let g:solarized_bold=1
let g:solarized_italic=1

set synmaxcol=1024
set background=dark
silent! colorscheme solarized

" ====
" Diff
" ====

set isfname-== " Remove '=' from filename characters
set diffopt+=iwhite " Add ignorance of whitespace to diff

" ===============
" Auto-completion
" ===============

set wildmenu
set wildchar=<Tab>
set wildmode=list:longest
set wildignore+=*.o,*.obj,.git,.svn
set wildignore+=*.png,*.jpg,*.jpeg,*.gif,*.mp3
set wildignore+=*/tmp/*,*.so,*.swp,*.zip
set complete=.,w,b,t
set pumheight=30
set completeopt=longest,menuone
set showfulltag

" =================================
" File encoding, encryption and EOL
" =================================

set viewoptions=unix,slash
set key=
set nobomb
set ffs=unix,dos,mac
set endofline
set encoding=utf-8
set termencoding=utf-8
set fileencodings=utf-8,iso-8859-15
setglobal fileencoding=utf-8

" =========
" Functions
" =========

" Motion for "next/last object" {{{

" https://github.com/sjl/dotfiles/blob/1f427dfe8f/vim/vimrc#L1415-1583

" Motion for "next/last object".  "Last" here means "previous", not "final".
" Unfortunately the "p" motion was already taken for paragraphs.
"
" Next acts on the next object of the given type, last acts on the previous
" object of the given type.  These don't necessarily have to be in the current
" line.
"
" Currently works for (, [, {, and their shortcuts b, r, B. 
"
" Next kind of works for ' and " as long as there are no escaped versions of
" them in the string (TODO: fix that).  Last is currently broken for quotes
" (TODO: fix that).
"
" Some examples (C marks cursor positions, V means visually selected):
"
" din'  -> delete in next single quotes                foo = bar('spam')
"                                                      C
"                                                      foo = bar('')
"                                                                C
"
" canb  -> change around next parens                   foo = bar('spam')
"                                                      C
"                                                      foo = bar
"                                                               C
"
" vin"  -> select inside next double quotes            print "hello ", name
"                                                       C
"                                                      print "hello ", name
"                                                             VVVVVV

onoremap an :<c-u>call <SID>NextTextObject('a', '/')<cr>
xnoremap an :<c-u>call <SID>NextTextObject('a', '/')<cr>
onoremap in :<c-u>call <SID>NextTextObject('i', '/')<cr>
xnoremap in :<c-u>call <SID>NextTextObject('i', '/')<cr>

onoremap al :<c-u>call <SID>NextTextObject('a', '?')<cr>
xnoremap al :<c-u>call <SID>NextTextObject('a', '?')<cr>
onoremap il :<c-u>call <SID>NextTextObject('i', '?')<cr>
xnoremap il :<c-u>call <SID>NextTextObject('i', '?')<cr>


function! s:NextTextObject(motion, dir)
	let c = nr2char(getchar())
	let d = ''

	if c ==# "b" || c ==# "(" || c ==# ")"
		let c = "("
	elseif c ==# "B" || c ==# "{" || c ==# "}"
		let c = "{"
	elseif c ==# "r" || c ==# "[" || c ==# "]"
		let c = "["
	elseif c ==# "'"
		let c = "'"
	elseif c ==# '"'
		let c = '"'
	else
		return
	endif

	" Find the next opening-whatever.
	execute "normal! " . a:dir . c . "\<cr>"
	
	if a:motion ==# 'a'
		" If we're doing an 'around' method, we just need to select around it
		" and we can bail out to Vim.
		execute "normal! va" . c
	else
		" Otherwise we're looking at an 'inside' motion.  Unfortunately these
		" get tricky when you're dealing with an empty set of delimiters because
		" Vim does the wrong thing when you say vi(.
		
		let open = ''
		let close = ''
		
		if c ==# "(" 
			let open = "("
			let close = ")"
		elseif c ==# "{"
			let open = "{"
			let close = "}"
		elseif c ==# "["
			let open = "\\["
			let close = "\\]"
		elseif c ==# "'"
			let open = "'"
			let close = "'"
		elseif c ==# '"'
			let open = '"'
			let close = '"'
		endif

		" We'll start at the current delimiter.
		let start_pos = getpos('.')
		let start_l = start_pos[1]
		let start_c = start_pos[2]
		
		" Then we'll find it's matching end delimiter.
		if c ==# "'" || c ==# '"'
			" searchpairpos() doesn't work for quotes, because fuck me.
			let end_pos = searchpos(open)
		else
			let end_pos = searchpairpos(open, '', close)
		endif

		let end_l = end_pos[0]
		let end_c = end_pos[1]
		
		call setpos('.', start_pos)
		
		if start_l == end_l && start_c == (end_c - 1)
			" We're in an empty set of delimiters.  We'll append an "x"
			" character and select that so most Vim commands will do something
			" sane.  v is gonna be weird, and so is y.  Oh well.
			execute "normal! ax\<esc>\<left>"
			execute "normal! vi" . c
		elseif start_l == end_l && start_c == (end_c - 2)
			" We're on a set of delimiters that contain a single, non-newline
			" character.  We can just select that and we're done.
			execute "normal! vi" . c
		else
			" Otherwise these delimiters contain something.  But we're still not
			" sure Vim's gonna work, because if they contain nothing but
			" newlines Vim still does the wrong thing.  So we'll manually select
			" the guts ourselves.
			let whichwrap = &whichwrap
			set whichwrap+=h,l
			
			execute "normal! va" . c . "hol"
			
			let &whichwrap = whichwrap
		endif
	endif
endfunction

" }}}
" Add a "number" motion object {{{

" Add's a number as a text object, so from '#123456' the number is everything
" except the '#', at least that's how I understand it.
"
" I got this excellent piece of VimL from here:
" http://sprunge.us/QTPL?vim

onoremap N :<c-u>call <SID>NumberTextObject(0)<CR>
xnoremap N :<c-u>call <SID>NumberTextObject(0)<CR>
onoremap aN :<c-u>call <SID>NumberTextObject(1)<CR>
xnoremap aN :<c-u>call <SID>NumberTextObject(1)<CR>
onoremap iN :<c-u>call <SID>NumberTextObject(1)<CR>
xnoremap iN :<c-u>call <SID>NumberTextObject(1)<CR>

function! s:NumberTextObject(whole)
	normal! v
	while getline('.')[col('.')] =~# '\v[0-9]'
		normal! l
	endwhile
	if a:whole
		normal! o
		while col('.') > 1 && getline('.')[col('.') - 2] =~# '\v[0-9]'
			normal! h
		endwhile
	endif
endfunction

" }}}
" SynStack() {{{

" This functly allows you to figure out the name of the text object you're on
" top of.
"
" I got this one from here:
" http://vimcasts.org/episodes/creating-colorschemes-for-vim/

function! <SID>SynStack()
	if !exists('*synstack')
		return
	endif
	echo map(synstack(line('.'), col('.')), 'synIDattr(v:val, "name")')
endfunc

nnoremap <C-S-p> :call <SID>SynStack()<CR>

" }}}
" ListWrapToggle() {{{

" The best way I've found of switching between line soft-wrap and showing
" hidden characters.
"
" I designed this myself.

function! ListWrapToggle()
	if(&list == 1)
		set nolist
		set wrap
	elseif(&wrap == 1)
		set nowrap
		set list
	else
		set nowrap
		set list
	endif
endfunction

nnoremap <silent> <leader>tw :call ListWrapToggle()<CR>

" }}}

" =============
" Auto commands
" =============

if (has('autocmd'))
	augroup cursorline
		autocmd!
		" Only show 'cursorline' in the current window and in normal mode
		au WinLeave,InsertEnter * set nocursorline
		au WinEnter,InsertLeave * set cursorline
		" Only show 'cursorcolumn' in current window and in normal mode
		au WinLeave,InsertEnter * set nocursorcolumn
		au WinEnter,InsertLeave * set cursorcolumn
	augroup END

	" Set to use manual folds in Vim files
	augroup filetype_vim
		autocmd!
		autocmd FileType vim setlocal foldmethod=marker
	augroup END

	" Specific settings for the Openbox rc.xml file
	augroup file_openboxrc
		autocmd!
		autocmd BufRead rc.xml setlocal expandtab
		autocmd BufRead rc.xml setlocal shiftwidth=2
		autocmd BufRead rc.xml setlocal tabstop=2
		autocmd BufRead rc.xml setlocal softtabstop=2
	augroup END

	" Some settings for fugitive.vim by Tim Pope
	augroup fugitive
		autocmd!
		autocmd BufReadPost fugitive://* set bufhidden=delete
	augroup END

	" Using `par` on Git commits and text files
	augroup par_settings
		autocmd!
		autocmd FileType text setlocal formatprg=par\ w79r
		autocmd FileType gitcommit setlocal formatprg=par\ w72r
	augroup END

	" Resize splits when window is resized
	au VimResized * :wincmd =

	" augroup VimReload
	" 	autocmd!
	" 	autocmd BufWritePost $MYVIMRC source $MYVIMRC
	" 	autocmd BufWritePost $MYGVIMRC source $MYGVIMRC
	" augroup END

	augroup StatuslineReload
		autocmd!
		autocmd BufWritePost ~/.vim/plugin/statusline.vim source
					\ ~/.vim/plugin/statusline.vim
	augroup END
endif

" vim: set nowrap fdm={{{,}}}

