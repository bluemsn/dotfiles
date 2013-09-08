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

" =========
" NeoBundle
" =========

" Set the filetype stuff to off, required for Vundle
filetype plugin indent off

 if has('vim_starting')
	set runtimepath+=~/.vim/bundle/neobundle.vim/
endif

call neobundle#rc(expand('~/.vim/bundle/'))

" Let Vundle handle itself as a bundle, REQUIRED!
NeoBundleFetch 'Shougo/neobundle.vim'

NeoBundle 'chip/vim-fat-finger'
NeoBundle 'chreekat/vim-paren-crosshairs'
NeoBundle 'drmikehenry/vim-fixkey'
NeoBundle "jiangmiao/auto-pairs"
NeoBundle 'jszakmeister/vim-togglecursor'
"NeoBundle 'kien/ctrlp.vim'
"---------------TRYING OUT
NeoBundle 'kien/rainbow_parentheses.vim'
NeoBundle 'matze/vim-move'
NeoBundle 'mikewest/vimroom'
NeoBundle 'rhysd/clever-f.vim'
NeoBundle 'scrooloose/nerdtree'
NeoBundle 'sjl/vitality.vim'
NeoBundle 'svermeulen/vim-easyclip'
"NeoBundle 'takac/vim-commandcaps'
NeoBundle 'tpope/vim-abolish'
NeoBundle 'tpope/vim-commentary'
NeoBundle 'tpope/vim-eunuch'
NeoBundle 'tpope/vim-fugitive'
NeoBundle 'tpope/vim-repeat'
NeoBundle 'tpope/vim-surround'
NeoBundle 'troydm/easybuffer.vim'
"NeoBundle 'zhaocai/GoldenView.Vim'

NeoBundle 'AndrewRadev/vim-eco'
NeoBundle 'cakebaker/scss-syntax.vim'
"NeoBundle 'groenewege/vim-less'
NeoBundle 'jelera/vim-javascript-syntax'
NeoBundle 'kchmck/vim-coffee-script'
NeoBundle 'kloppster/Wordpress-Vim-Syntax'
NeoBundle 'mutewinter/vim-css3-syntax'
"NeoBundle 'olivierverdier/python-syntax.vim'
NeoBundle 'othree/html5.vim'
NeoBundle 'othree/javascript-libraries-syntax.vim'
NeoBundle 'tpope/vim-git'
NeoBundle 'tpope/vim-markdown'
"NeoBundle 'wavded/vim-stylus'
NeoBundle 'zaiste/tmux.vim'

NeoBundle 'gorodinskiy/vim-coloresque'
NeoBundle 'gregsexton/MatchTag'
NeoBundle 'mattn/emmet-vim'
NeoBundle 'nelstrom/vim-markdown-folding'
NeoBundle 'spf13/PIV'

"NeoBundle 'chriskempson/base16-vim'
NeoBundle 'Greduan/vim-colors-solarized'
"NeoBundle 'molok/vim-vombato-colorscheme'
"NeoBundle 'nanotech/jellybeans.vim'
"NeoBundle 'tomasr/molokai'

NeoBundle 'Conque-Shell'
NeoBundle 'IndexedSearch'
"NeoBundle 'restore_view.vim'
NeoBundle 'sessionman.vim'

" Set the filetype stuff to on, no longer required off
filetype plugin indent on

" Installation check
NeoBundleCheck

" ===============
" Bundle settings
" ===============

" vim-move {{{
let g:move_key_modifier='S'
" }}}
" togglecursor {{{
let g:togglecursor_default='block' " Cursor for everything except insert mode
let g:togglecursor_insert='line' " Cursor for insert mode
let g:togglecursor_leave='block' " Cursor to set when leaving Vim
" }}}
" Indent Guides {{{
let g:indent_guides_color_change_percent=5 " % at which to change lightness
let g:indent_guides_guide_size=1 " Set width of marker to one wide
let g:indent_guides_start_level=1 " Level at which to start highlighting
let g:indent_guides_space_guides=1 " Make sure to recognize space tabs
let g:indent_guides_enable_on_vim_startup=1 " Load bundle at startup
" }}}
" neocomplcache {{{
let g:neocomplcache_enable_at_startup=1 " Enable at startup
" }}}
" todo.vim {{{
let g:TodoExplicitCommentsEnabled=1 " Enable explicit comments
hi! link TodoItemAdditionalText TodoItem
" }}}
" Vitality.vim {{{
if (!has('gui_running'))
	let g:vitality_always_assume_iterm=1
	let g:vitality_fix_cursor=0
endif
" }}}

" ===================
" Search and matching
" ===================

set iskeyword+=-    " Add '-' as a keyword
set wrapscan     " Set the search scan to wrap around to the top of the file
set ignorecase   " Set search scan to ignore case when search is all lowercase
set smartcase    " But recognize uppercase if it is specified
set novisualbell " Set to use visual bell --  the beeping sucks!
set noerrorbells " Enable use of (visual) error bells
set incsearch    " Show results of search scan as it finds them
set noshowmatch  " Disable show match, using matchparen instead
" Set the match pairs for matchparen
set matchpairs=(:),{:},[:],':',":"
set showcmd      " Show the current command in the lower right corner
set magic        " Allow use of regular expressions in the search scans

if (&t_Co > 2 || has('gui_running'))
	set hls
endif

if (has('win32') || has('win64'))
	" This is for Windows/cygwin and to add -H.
	" '$*' is not passed to the shell, but used by Vim.
	set grepprg=grep\ -nH\ $*\ /dev/null
endif

" Set this to 1 if you want to use Ack instead of Grep, or ag instead of Ack
let use_ack=0
let use_ag=0

" Check if I want to use Ack
if (use_ack != 0)
	" Use Ack instead of Grep when available
	if (executable('ack'))
		set grepprg=ack\ -H\ --nogroup\ --nocolor\ --ignore-dir=tmp\ --ignore-dir=coverage
	elseif (executable('ack-grep'))
		set grepprg=ack-grep\ -H\ --nogroup\ --nocolor\ --ignore-dir=tmp\ --ignore-dir=coverage
	endif
endif

set fo=vt         " Set the format options ('formatoptions')
set nojoinspaces  " :h joinspaces

" ===========
" Indentation
" ===========

set backspace=indent,eol,start " Backspace over everything in insert mode
set noexpandtab   " Make sure that every file uses real tabs, not spaces
set shiftround    " Round indent to multiple of 'shiftwidth'
set smartindent   " Do smart indenting when starting a new line
set autoindent    " Copy indent from current line, over to the new line

" Set the tab width
let s:tabwidth=4
exec 'set tabstop='    .s:tabwidth
exec 'set softtabstop='.s:tabwidth
exec 'set shiftwidth=' .s:tabwidth

" ================
" Command settings
" ================

set cpoptions+=$    " Default but put a '$' at the end of motion string

" =======
" History
" =======

set history=1000 " Keep {num} entries in the command history

" =====================
" Backup and undo files
" =====================

set backup     " Enable backup files
set swapfile   " Use a swap file in current buffer
set nowb       " Write backup before saving
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

set foldenable        " Make sure folding is enabled
set foldmethod=marker " Use manual markers for folds
set foldlevelstart=0  " Always close folds when switching buffers

" These commands open, or can open folds
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

nn <silent> <leader>eev :e $MYVIMRC<CR>
nn <silent> <leader>ev :sp $MYVIMRC<CR>
nn <silent> <leader>eeg :e $MYGVIMRC<CR>
nn <silent> <leader>eg :sp $MYGVIMRC<CR>
nn <silent> <leader>ees :e ~/.vim/plugin/statusline.vim<CR>
nn <silent> <leader>es :sp ~/.vim/plugin/statusline.vim<CR>
nn <silent> <leader>eec :e ~/.vim/plugin/tmux_navigator.vim<CR>
nn <silent> <leader>ec :sp ~/.vim/plugin/tmux_navigator.vim<CR>

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

" Smart way to move between windows, uses 'Ctrl+hjkl' instead of 'Ctrl+w+hjkl'
nn <C-j> <C-w>j
nn <C-k> <C-w>k
nn <C-h> <C-w>h
nn <C-l> <C-w>l

" Same as *, but doesn't move the cursor, only highlights
" http://twitter.com/dmedvinsky/status/109304047206547456
nn <silent> <leader>hh :setl hls<CR>:let @/="\\<<C-r><C-w>\\>"<CR>

" Preserve indentation while pasting text from the OS X clipboard
no <leader>p :set paste<CR>:put  *<CR>:set nopaste<cCR>

" Use ',z' to focus current fold, closing every other fold in the process
" http://twitter.com/dotvimrc/status/129979569045381120
nn <leader>z zMzvzz

" http://twitter.com/dotvimrc/status/132489424494792704
no H ^
no L g_

" Clean trailing white space
nn <leader>$ :call Preserve("%s/\\s\\+$//e")<CR>

nn <leader>w <C-w>

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

set cmdheight=2 " Make the command input line two lines high
set shellslash  " Set to use forward slash, in case you're in Windows
set showmode    " Always show the current mode
set showcmd     " Show (partial) command in the last line of screen
set report=0    " Report this or greater number of changes
set list                            " Show hidden characters
set listchars=tab:\|\ ,eol:$,trail:_ " Set chars to use for 'list'
set nowrap    " By default soft-wrap text at window border
set linebreak " Visual linebreak at 'breakat' rather than last char in window
set number " Line numbers
set relativenumber
set colorcolumn=79 " Put a marker in array of column numbers
set shortmess=astI " :h shortmess
set ttyfast     " Faster Terminal, redraws stuff quicker!
set linespace=0 " No extra spaces between text lines
set lazyredraw  " Don't update the display while executing macros
set laststatus=2 " Always use a statusline
set ruler        " Put a ruler, when my custom statusline doesn't load
set scrolloff=30    " How near the cursor can get to the top/bottom of window
set sidescrolloff=10 " Same as above, but for side scrolling
set sidescroll=1    " Minimal columns to scroll horizontally
set virtualedit=all " Allow the cursor to go to invalid places
set mousehide       " Hide the mouse pointer while typing
set mouse=          " Disable mouse

" =======================
" Window/split management
" =======================

set title          " Change Terminal's title
set fillchars=stl:\ ,stlnc:\ ,vert:\|,fold:-,diff:- " Set the various fill
                                                    " characters for stuff
set autowrite                " When switching buffers save file automatically
set autoread                 " Auto read files when edited outside Vim
set tabpagemax=1             " Max tabs to open with the '-p' option
set showtabline=0            " Don't show the Vim tab line
set switchbuf=useopen,usetab " Switch to tab/window if buffer is already open
" set winheight=3              " Just to avoid errors, don't pay attention here
" set winminheight=3           " Minimum window height (split window)
" set winheight=10             " Height current split should have

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
" Numbers {{{

" Motion for numbers. Great for CSS. Lets you do things like this:
"
" margin-top: 200px; -> daN -> margin-top: px;
"              ^                          ^
" TODO: Handle floats.

onoremap N :<c-u>call <SID>NumberTextObject(0)<cr>
xnoremap N :<c-u>call <SID>NumberTextObject(0)<cr>
onoremap aN :<c-u>call <SID>NumberTextObject(1)<cr>
xnoremap aN :<c-u>call <SID>NumberTextObject(1)<cr>
onoremap iN :<c-u>call <SID>NumberTextObject(1)<cr>
xnoremap iN :<c-u>call <SID>NumberTextObject(1)<cr>

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
" Preserve() {{{

" I got this one from here:
" http://vimcasts.org/episodes/tidying-whitespace/

function! Preserve(command)
	" Preparation: save last search, and cursor position.
	let _s=@/
	let l = line(".")
	let c = col(".")
	" Do the business:
	execute a:command
	" Clean up: restore previous search history, and cursor position
	let @/=_s
	call cursor(l, c)
endfunction

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

