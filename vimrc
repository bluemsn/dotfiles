" Fish {{{
if ($SHELL =~ 'fish')
	set shell=/bin/sh
endif
" }}}
" Basic settings {{{
set nocompatible
set hidden
set modeline
set modelines=5
set viminfo+=!
set history=1000
set cpoptions+=$

" http://twitter.com/mbadran/status/111011179907915776
set clipboard+=unnamed
set clipboard+=unnamedplus
" }}}
" Plugin stuff {{{
" http://crumbtrail.chesmart.in/post/5024677985/man-vim-dude
runtime! ftplugin/man.vim

" https://github.com/tpope/vim-sensible
if (!exists('g:loaded_matchit') && findfile('plugin/matchit.vim', &rtp) ==# '')
	runtime! macros/matchit.vim
endif

" vim-plug {{{
call plug#begin()

Plug 'mattn/emmet-vim'
"Plug 'kien/rainbow_parentheses.vim'
Plug 'tomtom/tcomment_vim'
Plug 'Shougo/unite.vim'
Plug 'tpope/vim-classpath'
Plug 'gorodinskiy/vim-coloresque'
Plug 'haya14busa/vim-easymotion'
Plug 'tpope/vim-fireplace'
Plug 'drmikehenry/vim-fixkey'
Plug 'osyo-manga/vim-over'
Plug 'chreekat/vim-paren-crosshairs'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-vinegar'
Plug 'justinmk/vim-sneak'
Plug 'tpope/vim-surround'
Plug 'morhetz/gruvbox'
Plug 'othree/html5.vim'
Plug 'cakebaker/scss-syntax.vim'
Plug 'zaiste/tmux.vim'
Plug 'guns/vim-clojure-static'
Plug 'kchmck/vim-coffee-script'
Plug 'mutewinter/vim-css3-syntax'
Plug 'AndrewRadev/vim-eco'
Plug 'tpope/vim-git'
Plug 'elzr/vim-json'
Plug 'tpope/vim-markdown'
Plug 'vim-scripts/Auto-Pairs'
Plug 'vim-scripts/IndexedSearch'
Plug 'Shougo/vimproc.vim'
"Plug 'Shougo/vimshell.vim'
Plug 'vim-scripts/bufkill.vim'
Plug 'vim-scripts/Smart-Tabs'
Plug 'scrooloose/nerdtree'
Plug 'vim-scripts/scratch.vim'
Plug 'sjl/clam.vim'
Plug 'terryma/vim-multiple-cursors'
Plug 'mhinz/vim-startify'
Plug 'mattn/gist-vim'
Plug 'chrisbra/NrrwRgn'
Plug 'bling/vim-airline'
Plug 'edkolev/tmuxline.vim'
Plug 'justinmk/vim-gtfo'
"Plug 'wakatime/vim-wakatime'
Plug 'reedes/vim-pencil'
Plug 'luochen1990/rainbow'

call plug#end()
" }}}

filetype plugin indent on

" Vitality {{{
let g:vitality_fix_cursor=0
" }}}
" MiniBufExpl {{{
let g:miniBufExplBuffersNeeded=0
" }}}
" Emmet {{{
let g:user_emmet_install_global=0
au FileType html,css EmmetInstall
au FileType html,css imap <expr> <tab> emmet#expandAbbrIntelligent("\<tab>")
" }}}
" Rainbow Parentheses {{{
"au VimEnter * RainbowParenthesesToggle
"au Syntax * RainbowParenthesesLoadRound
"au Syntax * RainbowParenthesesLoadSquare
"au Syntax * RainbowParenthesesLoadBraces
let g:rainbow_active=1
" }}}
" Unite {{{
" https://github.com/bling/dotvim/blob/0c9b4e7183/vimrc#L532-L581
call unite#filters#matcher_default#use(['matcher_fuzzy'])
call unite#filters#sorter_default#use(['sorter_rank'])
call unite#set_profile('files', 'smartcase', 1)
call unite#custom#source('line,outline','matchers','matcher_fuzzy')

let g:unite_data_directory='~/.vim/tmp/unite'
let g:unite_enable_start_insert=1
let g:unite_source_history_yank_enable=1
let g:unite_source_rec_max_cache_files=5000
let g:unite_prompt='> '

if executable('ag')
	let g:unite_source_grep_command='ag'
	let g:unite_source_grep_default_opts='--nocolor --nogroup -S -C4'
	let g:unite_source_grep_recursive_opt=''
elseif executable('ack')
	let g:unite_source_grep_command='ack'
	let g:unite_source_grep_default_opts='--no-heading --no-color -a -C4'
	let g:unite_source_grep_recursive_opt=''
endif

function! s:unite_settings()
	nmap <buffer> Q <plug>(unite_exit)
	nmap <buffer> <esc> <plug>(unite_exit)
	imap <buffer> <esc> <plug>(unite_exit)
endfunction
autocmd FileType unite call s:unite_settings()
" }}}
" Airline {{{
let g:airline#extensions#whitespace#enabled=0
" }}}
" Sneak {{{
let g:sneak#streak=1
" }}}
" vim-pencil {{{
let g:pencil#textwidth=80
let g:pencil#joinspaces=1
let g:pencil#cursorwrap=0
" }}}

" }}}
" Search and matching {{{
set iskeyword+=-
set wrapscan
set ignorecase
set smartcase
set novisualbell
set noerrorbells
set incsearch
set noshowmatch
set showcmd
set magic
set nojoinspaces

if (&t_Co > 2 || has('gui_running'))
	set hls
endif
" }}}
" Indentation {{{
set backspace=indent,eol,start
set noautoindent
set nocindent
set nosmartindent
set noexpandtab
set shiftround

let s:tabwidth=4
exec 'set tabstop='    .s:tabwidth
exec 'set softtabstop='.s:tabwidth
exec 'set shiftwidth=' .s:tabwidth
" }}}
" Backup and undo files {{{
set backupdir=~/.vim/tmp/backup//
set directory=~/.vim/tmp/swap//
set undodir=~/.vim/tmp/undo//

set backup
set nowritebackup
set backupskip=/tmp/*,/private/tmp/*"
set noswapfile
set undofile
" }}}
" Folds {{{
set foldenable
set foldmethod=marker
set foldlevelstart=0
set foldopen=block,insert,jump,mark,percent,quickfix,search,tag,undo
" }}}
" Mappings and re-mappings {{{
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

" I prefer very magic mode
nn / /\v
vn / /\v

" I can never remember these
no <up> <C-b>
no <down> <C-d>
" Completely disable the use of the arrow keys in normal and visual modes
no <left> <NOP>
no <right> <NOP>
ino <up> <NOP>
ino <down> <NOP>
ino <left> <NOP>
ino <right> <NOP>

" Center next search result
nn n nzzzv
nn N Nzzzv

" Fix moving line by line in a paragraph when soft wrap is on
nn j gj
nn k gk
vn j gj
vn k gk

" Don't move on match
nn * *<C-o>

" Preserve indentation while pasting
no <leader>p :set paste<CR>:put *<CR>:set nopaste<CR>

" Better help tags navigation (IMO)
nn <C-S-right> <C-]>
nn <C-S-left>  <C-t>

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

" Give the ñÑ in some keyboards a good use
nn ñ :w<CR>
nn <S-ñ> :wq!<CR>

" Plugin keymappings
" Unite {{{
" https://github.com/bling/dotvim/blob/0c9b4e7183/vimrc#L532-L581
nm <space> [unite]
nn [unite] <NOP>
nn <silent> [unite]<space> :<C-u>Unite -toggle -auto-resize -buffer-name=mixed file_rec/async buffer file_mru bookmark<CR><C-u>
nn <silent> [unite]f :<C-u>Unite -toggle -auto-resize -buffer-name=files file_rec/async<CR><C-u>
nn <silent> [unite]y :<C-u>Unite -buffer-name=yanks history/yank<CR>
nn <silent> [unite]l :<C-u>Unite -auto-resize -buffer-name=line line<CR>
nn <silent> [unite]b :<C-u>Unite -auto-resize -buffer-name=buffers buffer<CR>
nn <silent> [unite]/ :<C-u>Unite -no-quit -buffer-name=search grep:.<CR>
nn <silent> [unite]m :<C-u>Unite -auto-resize -buffer-name=mappings mapping<CR>
nn <silent> [unite]s :<C-u>Unite -quick-match buffer<CR>
" }}}

" }}}
" Screen drawing {{{
"set whichwrap+=<,>,h,l,[,]
set cmdheight=2
set shellslash
set showmode
set showcmd
set report=0
set nowrap
set list
set listchars=tab:\|\ ,eol:$,trail:_,extends:),precedes:(
set number
set relativenumber
set colorcolumn=80
set shortmess=astI
set ttyfast
set linespace=0
set lazyredraw
set laststatus=2
set scrolloff=10
set sidescrolloff=10
set sidescroll=1
set virtualedit=all
set mousehide
set mouse=
set ruler
"set statusline=[%n]\ %f\ %y%<\ %(%M%R%W%)%=[%l,%v]\ (%L,%p%%)
" }}}
" Window/split management {{{
set title
set titlestring=VIM:\ %-25.55F\ %a%r%m titlelen=70
set fillchars=stl:\ ,stlnc:\ ,vert:\|,fold:-,diff:-
set autowrite
set autoread
set tabpagemax=1
set showtabline=0
set switchbuf=useopen,usetab
" }}}
" Colors {{{
if (&t_Co > 2 || has('gui_running'))
	syntax on
endif
if ($TERM == 'rxvt-unicode' || $TERM == 'xterm-256color' || $TERM == 'screen-256color' || &t_Co >= 256)
	set t_Co=256
endif

set synmaxcol=1024
set background=dark

let g:gruvbox_italic=0

silent! colorscheme gruvbox
" }}}
" Diff {{{
set isfname-== " Remove '=' from filename characters
set diffopt+=iwhite " Add ignorance of whitespace to diff
" }}}
" Auto-completion {{{
set wildmenu
set wildchar=<Tab>
set wildmode=list:longest
set wildignore+=*.png,*.jpg,*.jpeg,*.gif,*.mp3
set wildignore+=*/tmp/*,*.so,*.swp,*.zip
set complete=.,w,b,t
set pumheight=30
set completeopt=longest,menuone
set showfulltag
" }}}
" File encoding, encryption and EOL {{{
set viewoptions=unix,slash
set key=
set nobomb
set ffs=unix,dos,mac
set endofline
set encoding=utf-8
set termencoding=utf-8
set fileencodings=utf-8,iso-8859-15
setglobal fileencoding=utf-8
" }}}
" Functions {{{
function! s:NumberTextObject(whole) " {{{
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
onoremap N :<c-u>call <SID>NumberTextObject(0)<CR>
xnoremap N :<c-u>call <SID>NumberTextObject(0)<CR>
onoremap aN :<c-u>call <SID>NumberTextObject(1)<CR>
xnoremap aN :<c-u>call <SID>NumberTextObject(1)<CR>
onoremap iN :<c-u>call <SID>NumberTextObject(1)<CR>
xnoremap iN :<c-u>call <SID>NumberTextObject(1)<CR>
" http://sprunge.us/QTPL?vim
" }}}
function! <SID>SynStack() " {{{
	if !exists('*synstack')
		return
	endif
	echo map(synstack(line('.'), col('.')), 'synIDattr(v:val, "name")')
endfunc
nnoremap <C-S-p> :call <SID>SynStack()<CR>
" http://vimcasts.org/episodes/creating-colorschemes-for-vim/
" }}}
function! MyFoldText() " {{{
	let line = getline(v:foldstart)

	let nucolwidth = &fdc + &number * &numberwidth
	let windowwidth = winwidth(0) - nucolwidth - 3
	let foldedlinecount = v:foldend - v:foldstart

	" expand tabs into spaces
	let onetab = strpart('          ', 0, &tabstop)
	let line = substitute(line, '\t', onetab, 'g')

	let line = strpart(line, 0, windowwidth - 2 -len(foldedlinecount))
	let fillcharcount = windowwidth - len(line) - len(foldedlinecount)
	return line . '…' . repeat(" ",fillcharcount) . foldedlinecount . '…' . ' '
endfunction
" I'm sorry I stole this from you Steve
set foldtext=MyFoldText()
" }}}
function! <SID>WriteMode() " {{{
        setlocal formatoptions=tcroj
        setlocal textwidth=80
        setlocal wrapmargin=0
endfunction
nn <leader>w :call <SID>WriteMode()<CR>
" }}}
function! FiletypeIndent(tab, size) " {{{
	" use a real tab or spaces?
	if (a:tab == 1)
		setlocal noexpandtab
	else
		setlocal expandtab
	endif
	" set the tab size
	exec 'setlocal tabstop='    .a:size
	exec 'setlocal softtabstop='.a:size
	exec 'setlocal shiftwidth=' .a:size
endfunction
" }}}
" }}}
" Auto commands {{{
if (has('autocmd'))
	" Resize splits when window is resized
	au VimResized * :wincmd =

	augroup filetypes
		autocmd!
		" plugins
		au BufEnter * if (winnr("$") == 1 && exists("b:NERDTreeType") && b:NERDTreeType == "primary") | q | endif
		au BufReadPost fugitive://* set bufhidden=delete
		au BufNewFile,BufRead *.cljs setlocal filetype=clojure
		au FileType json,clojure call FiletypeIndent(0,2)
		" docpad
		au BufNewFile,BufRead *.css.scss    setlocal filetype=scss
		au BufNewFile,BufRead *.html.md     setlocal filetype=markdown
		au BufNewFile,BufRead *.html.md.eco setlocal filetype=markdown
		au BufNewFile,BufRead *.md          syntax match Comment /\%^---\_.\{-}---$/
		au BufNewFile,BufRead *.html.md     syntax match Comment /\%^---\_.\{-}---$/
		au BufNewFile,BufRead *.html.md.eco syntax match Comment /\%^---\_.\{-}---$/

		au FileType vim       setlocal foldmethod=marker
	augroup END
endif
" }}}
