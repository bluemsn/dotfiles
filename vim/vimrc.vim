" Maintainer: Eduan Lavaque <eduan@snapsimpletech.com>
" Maintainer URL: https://github.com/Greduan
" Last Change: Apr 26th, 2013

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
set clipboard+=unnamed
set clipboard+=unnamedplus

" Make sure Vim has autocmd support
if (has('autocmd'))
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
endif

" }}}
" Runtime stuff {{{

" Enable use of the :Man command, for man pages, explained here:
" http://crumbtrail.chesmart.in/post/5024677985/man-vim-dude
runtime ftplugin/man.vim

" Load matchit.vim, but only if the user hasn't installed a newer version.
" Found this trick here: https://github.com/tpope/vim-sensible
if (!exists('g:loaded_matchit') && findfile('plugin/matchit.vim', &rtp) ==# '')
  runtime! macros/matchit.vim
endif

" }}}
" Vundle & other extensions (syntaxes, filetypes etc.) {{{

" Set the filetype stuff to off, required for Vundle
filetype plugin indent off

set rtp+=~/.vim/bundle/vundle/ " Add Vundle to the list of things to load
call vundle#rc() " Call a Vundle function... Probably loads Vundle itself

" Bundles {{{

" Let Vundle handle itself as a bundle, REQUIRED!
Bundle 'gmarik/vundle'

" General
"Bundle 'aaronbieber/quicktask'
"Bundle 'AndrewRadev/sideways.vim'
"Bundle 'benmills/vimux'
"Bundle 'bkad/CamelCaseMotion'
Bundle 'chip/vim-fat-finger'
Bundle 'chreekat/vim-paren-crosshairs'
"Bundle 'davidoc/taskpaper.vim'
""Bundle 'ervandew/supertab'
"Bundle 'gmarik/sudo-gui.vim'
"Bundle 'godlygeek/csapprox'
"Bundle 'godlygeek/tabular'
""Bundle 'goldfeld/vim-seek'
"Bundle 'greyblake/vim-preview'
Bundle 'Greduan/vim-numbertoggle'
Bundle 'Greduan/vim-usefulstatusline'
"Bundle 'hwrod/interactive-replace'
"Bundle 'jeetsukumaran/vim-buffergator'
"Bundle 'jistr/vim-nerdtree-tabs'
Bundle 'jszakmeister/vim-togglecursor'
""Bundle 'junegunn/vim-scroll-position'
"Bundle 'justincampbell/vim-eighties'
""Bundle 'kablamo/vim-git-log'
"Bundle 'kana/vim-arpeggio'
"Bundle 'kana/vim-fakeclip'
""Bundle 'kana/vim-textobj-indent'
""Bundle 'kbarrette/mediummode'
"Bundle 'kien/ctrlp.vim'
""Bundle 'kien/rainbow_parentheses.vim'
"Bundle 'kikijump/tslime.vim'
"Bundle 'Lokaltog/powerline'
"Bundle 'Lokaltog/vim-easymotion'
""Bundle 'lyokha/vim-xkbswitch'
"Bundle 'mattn/gist-vim'
""Bundle 'mbbill/VimExplorer'
"Bundle 'mhinz/vim-signify'
""Bundle 'mhinz/vim-tmuxify'
"Bundle 'mihaifm/vimpanel'
"Bundle 'mileszs/ack.vim'
""Bundle 'nathanaelkane/vim-command-w'
"Bundle 'nathanaelkane/vim-indent-guides'
Bundle 'nelstrom/vim-americanize'
"Bundle 'nelstrom/vim-docopen'
"Bundle 'nelstrom/vim-visual-star-search'
"Bundle 'neochrome/todo.vim'
"Bundle 'Peeja/vim-cdo'
""Bundle 'PotHix/Vimpress'
"Bundle 'rking/ag.vim'
""Bundle 'scrooloose/nerdcommenter'
Bundle 'scrooloose/nerdtree'
"Bundle 'scrooloose/syntastic'
""Bundle 'scrooloose/whitespace2.0.vim'
"Bundle 'Shougo/neocomplcache'
"Bundle 'SirVer/ultisnips'
"Bundle 'sjl/clam.vim'
"Bundle 'sjl/gundo.vim'
Bundle 'sjl/vitality.vim'
"Bundle 'suan/vim-instant-markdown'
"Bundle 'techlivezheng/vim-plugin-minibufexpl'
"Bundle 'teranex/jk-jumps.vim'
"Bundle 'terryma/vim-expand-region'
"Bundle 'terryma/vim-smooth-scroll'
Bundle 'Townk/vim-autoclose'
Bundle 'tpope/vim-abolish'
"Bundle 'tpope/vim-capslock'
"Bundle 'tpope/vim-characterize'
Bundle 'tpope/vim-commentary'
Bundle 'tpope/vim-endwise'
Bundle 'tpope/vim-eunuch'
Bundle 'tpope/vim-fugitive'
"Bundle 'tpope/vim-obsession'
"Bundle 'tpope/vim-pastie'
Bundle 'tpope/vim-repeat'
"Bundle 'tpope/vim-speeddating'
Bundle 'tpope/vim-surround'
Bundle 'tpope/vim-unimpaired'
"Bundle 'troydm/easybuffer.vim'
""Bundle 'tsaleh/vim-align'
"Bundle 'Valloric/YouCompleteMe'
""Bundle 'wikitopian/hardmode'
"Bundle 'xolox/vim-shell'
Bundle 'zhaocai/GoldenView.vim'

" Syntax files
Bundle 'cakebaker/scss-syntax.vim'
"Bundle 'groenewege/vim-less'
"Bundle 'kloppster/Wordpress-Vim-Syntax'
Bundle 'mutewinter/vim-css3-syntax'
Bundle 'olivierverdier/python-syntax.vim'
Bundle 'othree/html5.vim'
Bundle 'othree/javascript-libraries-syntax.vim'
Bundle 'pangloss/vim-javascript'
Bundle 'tpope/vim-git'
Bundle 'tpope/vim-markdown'
Bundle 'zaiste/tmux.vim'

" Language specific bundles
"Bundle 'ap/vim-css-color'
Bundle 'gregsexton/MatchTag'
"Bundle 'HTML-AutoCloseTag'
Bundle 'mattn/zencoding-vim'
Bundle 'nelstrom/vim-markdown-folding'
Bundle 'robmiller/vim-movar'
Bundle 'spf13/PIV'

" Color schemes
"Bundle 'chriskempson/base16-vim'
Bundle 'Greduan/vim-colors-solarized'
"Bundle 'molok/vim-vombato-colorscheme'
"Bundle 'nanotech/jellybeans.vim'
"Bundle 'tomasr/molokai'

" vim-scripts repos
"Bundle 'blinking_statusline.vim"
"Bundle 'bufkill.vim'
"Bundle 'Conque-Shell'
"Bundle 'IndexedSearch'
"Bundle 'progressbar-widget'
"Bundle 'restore_view.vim'
"Bundle 'SearchComplete'
"Bundle 'sessionman.vim'
"Bundle 'ShowMarks'
"Bundle 'YankRing.vim'
"Bundle 'VimRepress'

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
" neocomplcache {{{
let g:neocomplcache_enable_at_startup=1 " Enable at startup
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
if (!has('gui_running'))
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

if (&t_Co > 2 || has('gui_running'))
	set hls " Enable the highlighting of the search
endif

" Highlight VCS conflict markers
" match ErrorMsg '^\(<\|=\|>\)\{7\}\([^=].\+\)\?$'

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
exec 'set softtabstop='.s:tabwidth
exec 'set shiftwidth=' .s:tabwidth

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

" When you’re pressing Escape to leave insert mode in the terminal, it will by
" default take a second or another keystroke to leave insert mode completely
" and update the statusline. This fixes that. I got this from:
" https://powerline.readthedocs.org/en/latest/tipstricks.html#vim
set ttimeoutlen=2000
augroup FastEscape
	autocmd!
	au InsertEnter * set timeoutlen=0
	au InsertLeave * set timeoutlen=2000
augroup END

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

if (has('persistent_undo'))
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

" Nothing here yet...

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
set listchars=tab:\|\ ,eol:$,trail:_ " Set chars to use for 'list'

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
set winheight=3              " Just to avoid errors, don't pay attention here
set winminheight=3           " Minimum window height (split window)
set winheight=10             " Height current split should have

" Make sure Vim has autocmd support
if (has('autocmd'))
	au VimResized * :wincmd = " Resize split windows when the window is resized

	" Save all buffers when Vim loses focus
	augroup SaveAll
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

if (!has('gui_running'))
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
if (has('autocmd'))
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

	augroup par_settings
		autocmd!
		autocmd FileType text setlocal formatprg=par\ w79r
		autocmd FileType gitcommit setlocal formatprg=par\ w72r
	augroup END
endif

" }}}
" Functions {{{

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

" }}}
" Random stuff {{{

set isfname-== " Remove '=' from filename characters

" }}}
" Tips {{{

" To show all help topics containing 'help'
" :h word<CTRL-d>

" To open a certain URL in Vim
" $ vim $URL

" To output the current file in HTML. It comes along with syntax highlighting.
" :%TOhtml

" Useful mappings:
"
" ZZ     - save and close current file
" zz     - makes current line the center of editor
" cc, S  - cuts current line and puts you in insert mode

" }}}

" vim: set nowrap fdm={{{,}}}
