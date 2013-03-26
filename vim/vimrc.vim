" My personal .vimrc file, containing lots of stuff which make it perfect for
" my tastes. I suggest you don't use this yourself, but rather read it and
" take what you like for yourself.
"
" Maintainer: Eduan Lavaque <eduan@snapsimpletech.com>
" Maintainer URL: https://github.com/Greduan
" Last Change: March 24th, 2013
"
" If you insist on using it, simply put this file here, depending on your OS:
"
" Unix:    ~/.vimrc
" Mac OSX: ~/.vimrc
" Amiga:   s:.vimrc
" Linux:   /home/$USER/.vimrc
" DOS:     $VIM\_vimrc
" OpenVMS: sys$login:.vimrc

" Basic stuff {{{

" Use Vim settings, rather then Vi settings (much better!).
" This must be first, because it changes other options as a side effect.
set nocompatible

set hidden                 " Keep changed buffers without requiring saves
set viewoptions=unix,slash " Better Unix/Windows compatibility
set modeline               " Allow file specific Vim settings
set viminfo+=!             " Keep global uppercase variables
set noesckeys              " I don't use any mappings starting with <Esc>

" Use the system's clipboard
" http://twitter.com/mbadran/status/111011179907915776
set clipboard^=unnamed

" Make sure Vim has autocmd support
if has('autocmd')
	" Reload .vimrc and .gvimrc files as soon as they have a change
	augroup VimReload
		autocmd!
		autocmd BufWritePost $MYVIMRC source $MYVIMRC
		autocmd BufWritePost $MYGVIMRC source $MYGVIMRC
	augroup END

	" Reload statusline.vim file as soon as it changes
	augroup StatuslineReload
		autocmd!
		autocmd BufWritePost ~/.vim/plugin/statusline.vim source
			\ ~/.vim/plugin/statusline.vim
	augroup END

	" Reload cursor settings for tmux/iTerm as soon as it changes
	augroup tmuxiTermReload
		autocmd!
		autocmd BufWritePost ~/.vim/after/plugin/tmux_iterm.vim source
			\ ~/.vim/after/plugin/tmux_iterm.vim
	augroup END
endif

" }}}
" Runtime stuff {{{

" Enable use of the :Man command, for man pages, explained here:
" http://crumbtrail.chesmart.in/post/5024677985/man-vim-dude
runtime ftplugin/man.vim

" Enable extended '%' matching
runtime macros/matchit.vim

" }}}
" Vundle & other extensions (syntaxes, filetypes etc.) {{{

" Set the filetype stuff to off, required for Vundle
filetype plugin indent off

set rtp+=~/.vim/bundle/vundle/ " Add Vundle to the list of things to load
call vundle#rc() " Call a Vundle function... Probably loads Vundle itself

" All of my Vundle bundles {{{

" Bundles {{{

" Let Vundle handle itself as a bundle, REQUIRED!
Bundle 'gmarik/vundle'

" My own bundles
Bundle 'Greduan/vim-colors-solarized'
Bundle 'Greduan/vim-numbertoggle'
Bundle 'Greduan/vim-usefulstatusline'

" General
Bundle 'airblade/vim-gitgutter'
"Bundle 'AndrewRadev/sideways.vim'
Bundle 'chip/vim-fat-finger'
Bundle 'chreekat/vim-paren-crosshairs'
"Bundle 'ervandew/supertab'
Bundle 'gmarik/sudo-gui.vim'
"Bundle 'godlygeek/csapprox'
"Bundle 'goldfeld/vim-seek'
Bundle 'jszakmeister/vim-togglecursor'
"Bundle 'Lokaltog/vim-easymotion'
Bundle 'nathanaelkane/vim-indent-guides'
"Bundle 'neochrome/todo.vim'
"Bundle 'PotHix/Vimpress'
Bundle 'sjl/vitality.vim'
"Bundle 'techlivezheng/vim-plugin-minibufexpl'
Bundle 'Townk/vim-autoclose'
Bundle 'tpope/vim-endwise'
Bundle 'tpope/vim-fugitive'
Bundle 'tpope/vim-repeat'
Bundle 'tpope/vim-surround'
Bundle 'tpope/vim-unimpaired'
Bundle 'Valloric/YouCompleteMe'

" Syntax files
Bundle 'groenewege/vim-less'
Bundle 'mutewinter/vim-css3-syntax'
Bundle 'olivierverdier/python-syntax.vim'
Bundle 'othree/html5.vim'
Bundle 'othree/javascript-libraries-syntax.vim'
Bundle 'pangloss/vim-javascript'
Bundle 'tpope/vim-git'
Bundle 'tpope/vim-markdown'
Bundle 'zaiste/tmux.vim'

" Language specific bundles
Bundle 'HTML-AutoCloseTag'
Bundle 'nelstrom/vim-markdown-folding'
Bundle 'robmiller/vim-movar'
Bundle 'skammer/vim-css-color'
Bundle 'spf13/PIV'

" Color schemes
"Bundle 'chriskempson/base16-vim'
"Bundle 'nanotech/jellybeans.vim'
Bundle 'tomasr/molokai'

" vim-scripts repos
Bundle 'SearchComplete'
"Bundle 'YankRing.vim'

" }}}
" To check out {{{

"Bundle 'aaronbieber/quicktask'
"Bundle 'ap/vim-css-color'
"Bundle 'benmills/vimux'
"Bundle 'bkad/CamelCaseMotion'
"Bundle 'godlygeek/tabular'

"Bundle 'davidoc/taskpaper.vim'
"Bundle 'gregsexton/MatchTag'
"Bundle 'greyblake/vim-preview'
"Bundle 'hlissner/vim-multiedit'
"Bundle 'hwrod/interactive-replace'
"Bundle 'jeetsukumaran/vim-buffergator'
"Bundle 'jistr/vim-nerdtree-tabs'
"Bundle 'junegunn/vim-scroll-position'
"Bundle 'kana/vim-arpeggio'
"Bundle 'kana/vim-fakeclip'
"Bundle 'kana/vim-textobj-indent'
"Bundle 'kien/ctrlp.vim'
"Bundle 'kien/rainbow_parentheses.vim'
"Bundle 'kikijump/tslime.vim'
"Bundle 'Lokaltog/powerline'
"Bundle 'mattn/gist-vim'
"Bundle 'mattn/zencoding-vim'
"Bundle 'mbbill/VimExplorer'
"Bundle 'mihaifm/vimpanel'
"Bundle 'mileszs/ack.vim'
"Bundle 'moshen/vim-superstatus'
"Bundle 'nathanaelkane/vim-command-w'
"Bundle 'nebelschwade/statusline-vim'
"Bundle 'nelstrom/vim-docopen'
"Bundle 'nelstrom/vim-visual-star-search'
"Bundle 'nfd/filepirate'
"Bundle 'Peeja/vim-cdo'
"Bundle 'rking/ag.vim'
"Bundle 'scrooloose/nerdcommenter'
"Bundle 'scrooloose/nerdtree'
"Bundle 'scrooloose/syntastic'
"Bundle 'scrooloose/whitespace2.0.vim'
"Bundle 'Shougo/neocomplcache'
"Bundle 'SirVer/ultisnips'
"Bundle 'sjl/clam.vim'
"Bundle 'sjl/friendly-find'
"Bundle 'sjl/gundo.vim'
"Bundle 'teranex/jk-jumps.vim'
"Bundle 'tpope/vim-abolish'
"Bundle 'tpope/vim-capslock'
"Bundle 'tpope/vim-commentary'
"Bundle 'tpope/vim-eunuch'
"Bundle 'tpope/vim-obsession'
"Bundle 'tpope/vim-pastie'
"Bundle 'tpope/vim-speeddating'
"Bundle 'troydm/easybuffer.vim'
"Bundle 'tsaleh/vim-align'
"Bundle 'wikitopian/hardmode'
"Bundle 'xolox/vim-shell'

" Colorschemes
"Bundle 'molok/vim-vombato-colorscheme'

" vim-scripts repos
"Bundle 'blinking_statusline.vim"
"Bundle 'bufkill.vim'
"Bundle 'Conque-Shell'
"Bundle 'IndexedSearch'
"Bundle 'progressbar-widget'
"Bundle 'restore_view.vim'
"Bundle 'sessionman.vim'
"Bundle 'ShowMarks'
"Bundle 'VimRepress'

" }}}

" }}}
" Bundle settings {{{

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
" todo.vim {{{
let g:TodoExplicitCommentsEnabled=1 " Enable explicit comments
hi! link TodoItemAdditionalText TodoItem
" }}}
" Git Branch Info {{{
let g:git_branch_status_text=' ' " Add a space before info
let g:git_branch_status_head_current=1 " Only show current branch
let g:git_branch_status_nogit='' " Message when there's no Git repo
let g:git_branch_status_around='()' " Enclose the branch in between these
let g:git_branch_file_based=1 " Check the file for info, instead of directory
" }}}
" MiniBufExplorer {{{
let g:miniBufExplMapCTabSwitchBufs=1
let g:miniBufExplUseSingleClick=1
let g:miniBufExplForceSyntaxEnable=1
" }}}
" numbertoggle {{{
let g:numbertoggle_defaultmodeoff='number'
let g:NumberToggleOff='<leader>tnO'
let g:NumberToggleTrigger='<leader>tn'
let g:NumberToggleOn='<leader>tno'
" }}}
" Vitality.vim {{{
if !has('gui_running')
	let g:vitality_always_assume_iterm=1
	let g:vitality_fix_cursor=0
endif
" }}}

" }}}

" Set the filetype stuff to on, no longer required off
filetype plugin indent on

" }}}
" Search & matching {{{

set wrapscan     " Set the search scan to wrap around to the top of the file
set ignorecase   " Set search scan to ignore case when search is all lowercase
set smartcase    " But recognize uppercase if it is specified
set novisualbell " Set to use visual bell --  the beeping sucks!
set noerrorbells " Enable use of (visual) error bells
set incsearch    " Show results of search scan as it finds them
set noshowmatch  " Disable show match, using matchparen instead
" Set the match pairs for matchparen
set matchpairs=(:),{:},[:],':',":",<:>
set showcmd      " Show the current command in the lower right corner
set magic        " Allow use of regular expressions in the search scans

if &t_Co > 2 || has('gui_running')
	set hls " Enable the highlighting of the search
endif

" Highlight VCS conflict markers
" match ErrorMsg '^\(<\|=\|>\)\{7\}\([^=].\+\)\?$'

if has('win32') || has('win64')
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
	if executable('ack')
		set grepprg=ack\ -H\ --nogroup\ --nocolor\ --ignore-dir=tmp\ --ignore-dir=coverage
	elseif executable('ack-grep')
		set grepprg=ack-grep\ -H\ --nogroup\ --nocolor\ --ignore-dir=tmp\ --ignore-dir=coverage
	endif
endif

" }}}
" Formatting {{{

set noexpandtab   " Make sure that every file uses real tabs, not spaces
set shiftround    " Round indent to pultiple of 'shiftwidth'
set backspace=indent,eol,start " Backspace over everything in insert mode
set smartindent   " Do smart indenting when starting a new line
set autoindent    " Copy indent from current line, over to the new line
set fo=vt         " Set the format options ('formatoptions')
set nojoinspaces  " :h joinspaces

" Set the tab width
let s:tabwidth=4
exec 'set tabstop='    .s:tabwidth
exec 'set shiftwidth=' .s:tabwidth
exec 'set softtabstop='.s:tabwidth

if (has('autocmd'))
	augroup par_settings
		autocmd!
		autocmd FileType text setlocal formatprg=par\ w79r
		autocmd FileType gitcommit setlocal formatprg=par\ w72r
	augroup END
endif

" }}}
" Commands' options {{{

set cpoptions+=$    " Default but put a '$' at the end of motion string
set iskeyword+=-    " Add '-' as a keyword

set timeout         " Do time out on mappings and others
set timeoutlen=2000 " Wait {num} ms before timing out a mapping

" When you’re pressing Escape to leave insert mode in the terminal, it will by
" default take a second or another keystroke to leave insert mode completely
" and update the statusline. This fixes that. I got this from:
" https://powerline.readthedocs.org/en/latest/tipstricks.html#vim
if !has('gui_running')
	set ttimeoutlen=2000
	augroup FastEscape
		autocmd!
		au InsertEnter * set timeoutlen=0
		au InsertLeave * set timeoutlen=2000
	augroup END
endif

" }}}
" Command line options {{{

set cmdheight=2 " Make the command input line two lines high
set shellslash  " Set to use forward slash, in case you're in Windows
set showmode    " Always show the current mode
set showcmd     " Show (partial) command in the last line of screen
set report=0    " Report this or greater number of changes

" }}}
" History {{{

set history=1000 " Keep {num} entries in the command history

" }}}
" Backups {{{

set backup     " Enable backup files
set swapfile   " Use a swap file in current buffer
set nowb       " Write backup before saving

" Set backup directory
set backupdir=~/.vim/tmpdir
set directory=~/.vim/tmpdir

" Create backup directory if it doesn't exist
if (!isdirectory(expand(&backupdir)))
	call mkdir(expand(&backupdir), 'p')
endif

" }}}
" Undo {{{

if has('persistent_undo')
	set undofile " Enable persistent undo

	" Set persistent undo directory
	set undodir=~/.vim/undodir
	" Create undo directory if it doesn't exist
	if (!isdirectory(expand(&undodir)))
		call mkdir(expand(&undodir), 'p')
	endif
endif

" }}}
" Folds {{{

set foldenable        " Make sure folding is enabled
set foldmethod=marker " Use manual markers for folds
set foldlevelstart=0  " Always close folds when switching buffers

" These commands open, or can open folds
set foldopen=block,insert,jump,mark,percent,quickfix,search,tag,undo

" }}}
" Keyboard {{{

" Keymappings {{{

" Leader key(s) {{{

" Set Leader key to ',', instead of '/'
let mapleader=','

" Set my local Leader key to '\'
let maplocalLeader = '\\'

" Map CTRL-E to do what ',' used to do
nnoremap <C-e> ,
vnoremap <C-e> ,

" }}}
" Edit files mappings {{{

" Open .vimrc file as a buffer in this split window
nnoremap <silent> <leader>eev :e $MYVIMRC<CR>

" Open .vimrc file in a vertical split
nnoremap <silent> <leader>ev :sp $MYVIMRC<CR>

" Open .gvimrc file as a buffer in this split window
nnoremap <silent> <leader>eeg :e $MYGVIMRC<CR>

" Open .gvimrc file in a vertical split
nnoremap <silent> <leader>eg :sp $MYGVIMRC<CR>

" Open statusline.vim file as a buffer in this split window
nnoremap <silent> <leader>ees :e ~/.vim/plugin/statusline.vim<CR>

" Open statusline.vim file in a vertical split
nnoremap <silent> <leader>es :sp ~/.vim/plugin/statusline.vim<CR>

" Open the tmux_iterm.vim file as a buffer in this split window
nnoremap <silent> <leader>eec :e ~/.vim/after/plugin/tmux_iterm.vim<CR>

" Open tmux_iterm.vim file in a vertical split
nnoremap <silent> <leader>ec :sp ~/.vim/after/plugin/tmux_iterm.vim<CR>

" }}}
" Arrow keys and 'hjkl' keys {{{

" Completely disable the use of the arrow keys in command and visual modes
noremap <up> <NOP>
noremap <down> <NOP>
noremap <left> <NOP>
noremap <right> <NOP>

" Fix moving line by line in the paragraph, when soft wrap is on
nnoremap j gj
nnoremap k gk
vnoremap j gj
vnoremap k gk

" Smart way to move between windows, uses 'Ctrl+hjkl' instead of 'Ctrl+w+hjkl'
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-h> <C-w>h
nnoremap <C-l> <C-w>l

" }}}
" Leader key mappings {{{

" Makes it easy to clear out a search, by typing ',<space>'
nnoremap <leader><space> :noh<CR>

" Same as *, but doesn't move the cursor, only highlights
" http://twitter.com/dmedvinsky/status/109304047206547456
nnoremap <silent> <leader>hh :setl hls<CR>:let @/="\\<<C-r><C-w>\\>"<CR>

" Preserve indentation while pasting text from the OS X clipboard
noremap <leader>p :set paste<CR>:put  *<CR>:set nopaste<cCR>

" Use ',z' to focus current fold, closing every other fold in the process
" http://twitter.com/dotvimrc/status/129979569045381120
nnoremap <leader>z zMzvzz

" Use capital H/L to first/last non-whitespace character
" http://twitter.com/dotvimrc/status/132489424494792704
noremap H ^
noremap L g_

" Call Preserve() and delete any trailing white space in buffer
nnoremap <leader>$ :call Preserve("%s/\\s\\+$//e")<CR>

" Make window controls easy
nnoremap <leader>w <C-w>

" }}}
" Ctrl mappings {{{

" Better help tags navigation (IMO)
nnoremap <C-S-Right> <C-]>
nnoremap <C-S-Left>  <C-t>

" }}}
" Re-mappings {{{

" Indent in visual and select mode automatically re-selects
vnoremap > >gv
vnoremap < <gv

" Select (charwise) the contents of the current line, excluding indentation
" http://twitter.com/dotvimrc/status/155748943001694208
nnoremap vv ^vg_

" Visually select the text that was last edited/pasted
" http://vimcasts.org/episodes/bubbling-text/
nnoremap gV `[v`]

" Bubble single lines
" http://vimcasts.org/episodes/bubbling-text/
nmap <S-up> [e
nmap <S-down> ]e

" Bubble multiple lines
" http://vimcasts.org/episodes/bubbling-text/
vmap <S-up> [egv
vmap <S-down> ]egv

" Fix the '&' command in normal and visual modes
" https://github.com/nelstrom/dotfiles/blob/d245b5cf67/vimrc#L99-L101
nnoremap & :&&<CR>
xnoremap & :&&<CR>

" Make 'Y' behave like 'D' and 'C'
" https://github.com/blueyed/dotfiles/blob/4407ba7905/vimrc#L1129-L1131
nnoremap Y y$
xnoremap Y y$

" <C-e> and <C-y> scroll the viewport a single line, bump this up a bit
nnoremap <C-e> 3<C-e>
nnoremap <C-y> 3<C-y>

" }}}
" Function keys mappings {{{

" Allow change of theme from light to dark and vice-versa, with a hotkey
call togglebg#map('<F5>')

" Disable pressing 'F1' for help, and set it equal to Escape
noremap <F1> <esc>

" }}}
" New mappings {{{

Nothing here yet...

" }}}

" }}}
" Special CAPS lock {{{

" http://vim.wikia.com/wiki/Insert-mode_only_Caps_Lock

" Execute 'lnoremap x X' and 'lnoremap X x' for each letter a-z
for c in range(char2nr('A'), char2nr('Z'))
  execute 'lnoremap ' . nr2char(c+32) . ' ' . nr2char(c)
  execute 'lnoremap ' . nr2char(c) . ' ' . nr2char(c+32)
endfor

" Kill the capslock when leaving insert mode
autocmd InsertLeave * set iminsert=0

" Use <F6> instead of <C-^> for toggleing CAPS lock
noremap <F6> :let &l:imi = !&l:imi<CR>
inoremap <F6> <C-o>:let &l:imi = !&l:imi<CR>
cnoremap <F6> <C-^>

" }}}

" }}}
" Abbreviations {{{

" Still gotta put stuff here

" }}}
" UI related {{{

" Enable list by default, but set it's options for when it is used
set list                            " Show hidden characters
set listchars=tab:▸\ ,eol:¬,trail:_ " Set chars to use for 'list'

" Disable softwrap by default, and set it's options, even if disabled
set nowrap    " By default soft-wrap text at window border
set linebreak " Visual linebreak at 'breakat' rather than last char in window

set title          " Change Terminal's title
set colorcolumn=79 " Put a marker in array of column numbers
set shortmess=astI " :h shortmess

" NOTICE!
" Line numbers are controlled by this Vundle bundle:
" https://github.com/Greduan/vim-numbertoggle

" Screen redrawing {{{

set ttyfast     " Faster Terminal, redraws stuff quicker!
set linespace=0 " No extra spaces between text lines
set lazyredraw  " Don't update the display while executing macros

" }}}
" Status line {{{

set laststatus=2 " Always use a statusline 
set ruler        " Put a ruler, when my custom statusline doesn't load

" The Vim statusline is set in a plugin that loads after the .vimrc has been
" loaded, otherwise we can't if certain plugins exist and stuff like that. The
" file that defines the statusline can be found under
" "~/.vim/plugin/statusline.vim"

" }}}
" Cursor & mouse {{{

set scrolloff=999   " Keep the cursor in the middle of the window
"set scrolloff=15    " How near the cursor can get to the top/bottom of window
set sidescrolloff=4 " Same as above, but for side scrolling
set sidescroll=1    " Minimal columns to scroll horizontally
set virtualedit=all " Allow the cursor to go to invalid places
set mousehide       " Hide the mouse pointer while typing
set mouse=          " Disable mouse

augroup cursorline
	autocmd!

	" Only show 'cursorline' in the current window and in normal mode
	au WinLeave,InsertEnter * set nocursorline
	au WinEnter,InsertLeave * set cursorline

	" Only show 'cursorcolumn' in current window and in normal mode
	au WinLeave,InsertEnter * set nocursorcolumn
	au WinEnter,InsertLeave * set cursorcolumn
augroup END

" }}}
" Windows/Split-windows {{{

set fillchars=stl:\ ,stlnc:\ ,vert:\|,fold:-,diff:- " Set the various fill
                                                    " characters for stuff
set autowrite                " When switching buffers save file automatically
set autoread                 " Auto read files when edited outside Vim
set tabpagemax=1             " Max tabs to open with the '-p' option
set showtabline=0            " Don't show the Vim tab line
set switchbuf=useopen,usetab " Switch to tab/window if buffer is already open
set winminheight=5           " Minimum window height (split window)
set winheight=10             " Height current split should have

" Make sure Vim has autocmd support
if has('autocmd')
	au VimResized * :wincmd = " Resize split windows when the window is resized

	" Save all buffers when Vim loses focus
	augroup saveall
		autocmd!

		au FocusLost * :wa
		au WinLeave * :wa
	augroup End
endif

" }}}
" Syntax highlighting {{{

if (&t_Co > 2 || has('gui_running'))
	" Switch syntax highlighting on, when the Terminal has colors
	" Or when the GUI is being used
	syntax on
endif

set synmaxcol=1024 " Max chars to highlight in a single, long line

if !has('gui_running')
	" Enable Terminal transparency
	let g:solarized_termtrans=1

	if (&t_Co >= 256 || $TERM == 'xterm-256color')
		" Do nothing, it handles itself.
	else
		" Make Solarized use 16 colors for Terminal support
		let g:solarized_termcolors=16
	endif
endif

" Leave this at normal at all times
let g:solarized_contrast='normal'

" Non-text items visibility, normal low or high
let g:solarized_visibility='normal'

" Show trailing white spaces
let g:solarized_hitrail=1

" Disable the Solarized menu, when using GUI
let g:solarized_menu=0

" Allow Solarized to use all the styles
let g:solarized_underline=1
let g:solarized_bold=1
let g:solarized_italic=1

set background=dark " Use the light/dark version the color scheme
silent! colorscheme solarized " Set the color scheme to use, no errors allowed

" }}}

" }}}
" Diff {{{

set diffopt+=iwhite " Add ignorance of whitespace to diff

" }}}
" Command line auto-completion {{{

set wildmenu              " Better command line auto-completion
set wildchar=<Tab>        " Set char to trigger wild-card expansion in
                          " command line
set wildmode=list:longest " Settings for when wildchar is used

" Ignore the following stuff when expanding wildcards
set wildignore+=*.o,*.obj,.git,.svn
set wildignore+=*.png,*.jpg,*.jpeg,*.gif,*.mp3
set wildignore+=*.sw?

" }}}
" Auto-completion {{{

set complete=.,w,b,t " Define how keyword auto-completion in insert mode
                     " should work
set pumheight=15     " Max lines to show in auto-complete box
set completeopt=longest,menuone " Settings for auto-completion
set showfulltag      " Show whole tag, not just function name, when
                     " autocompleting by tag

" }}}
" Macros {{{

" Nothing here yet...

" }}}
" File/Buffer encryption, encoding etc. {{{

set key=             " Disable encryption file and buffer encryption 
set nobomb           " Don't use BOMs (Byte Order Marks)
set ffs=unix,dos,mac " Set filetype to Unix, Windows and then Mac (Power PC)
set endofline        " Always add a EOL to every file

" Set the buffer encoding to be UTF-8
set encoding=utf-8
set termencoding=utf-8
set fileencodings=utf-8,iso-8859-15
setglobal fileencoding=utf-8

" }}}
" File type specific stuff {{{

" Make sure Vim has autocmd support
if has('autocmd')
	" Set to use manual folds in Vim files
	augroup filetype_vim
		autocmd!
		autocmd FileType vim setlocal foldmethod=marker
	augroup END

	" Python specific stuff
	augroup filetype_python
		autocmd!
		autocmd FileType python setlocal expandtab
	augroup END

	" Some settings for fugitive.vim by Tim Pope
	augroup fugitive
		autocmd!
		autocmd BufReadPost fugitive://* set bufhidden=delete
	augroup END

	augroup par_settings
		autocmd!
		autocmd FileType text setlocal formatprg=par\ w79r
		autocmd FileType gitcommit setlocal formatprg=par\ w72r
	augroup END
endif " if has('autocmd')

" }}}
" Functions {{{

" Add a "number" text object {{{

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
" I designed this myself, here's the latest Gist:
" https://gist.github.com/4210145

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
" Recording() {{{

" This is a function I made myself in order to setup Vim for recording, either
" HD or SD. It just changes window size.
"
" I designed this myself, here's the latest Gist:
" https://gist.github.com/4217374

function! Recording(quality)
	if(a:quality == 'sd')
		set guifont=Monaco:h11

		set columns=89
		set lines=28

		if(g:colors_name == 'solarized')
			set background=light
		endif
	elseif(a:quality == 'hd')
		set guifont=Monaco:h11

		set columns=180
		set lines=44

		if(g:colors_name == 'solarized')
			set background=light
		endif
	elseif(a:quality == 'off')
		if ! $diff " Check if in diff mode
			" If not, do a normal sized window
			set columns=120 lines=40 " Set the width and height of window
		else
			" If yes, then double that for diff mode
			set columns=240 lines=40 " Same here, duh!
		endif

		if(g:colors_name == 'solarized')
			set background=dark
		endif
	endif
endfunction

nnoremap <silent> <leader>trs :call Recording('sd')<CR>
nnoremap <silent> <leader>trh :call Recording('hd')<CR>
nnoremap <silent> <leader>tr :call Recording('off')<CR>

" }}}

" }}}
" Random stuff {{{

set isfname-== " Remove '=' from filename characters

" }}}
" Tips {{{

" To show all help topics containing 'help'
" :h word<CTRL-d>

" To open a certain URL in Vim
" $ vim http://eduantech.com

" To output the current file in HTML
" :%TOhtml

" Useful mappings:
"
" ZZ     - save and close current file
" zz     - makes current line the center of editor
" cc, S  - cuts current line and puts you in insert mode

" }}}

" vim: set nowrap fdm={{{,}}}
