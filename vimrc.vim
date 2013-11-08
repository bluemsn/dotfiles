" Basic settings {{{
set nocompatible
set hidden
set nomodeline
set modelines=0
set viminfo+=!
set history=1000
set cpoptions+=$

" http://twitter.com/mbadran/status/111011179907915776
set clipboard+=unnamed
set clipboard+=unnamedplus
" }}}
" Pathogen and plugin settings {{{
" http://crumbtrail.chesmart.in/post/5024677985/man-vim-dude
runtime! ftplugin/man.vim

" https://github.com/tpope/vim-sensible
if (!exists('g:loaded_matchit') && findfile('plugin/matchit.vim', &rtp) ==# '')
	runtime! macros/matchit.vim
endif

"let g:pathogen_disabled=['vim-markdown', 'html5.vim']

" Execute Pathogen
execute pathogen#infect()
filetype plugin indent on
filetype indent off

" Vitality.vim {{{
let g:vitality_fix_cursor=0
" }}}
" CtrlP {{{
let g:ctrlp_custom_ignore='\.git\|.svn\|\.hg\|node_modules'
" }}}
" MiniBufExpl {{{
let g:miniBufExplBuffersNeeded=0
" }}}
" Emmet.vim {{{
let g:user_emmet_install_global=0
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

if (has('win32') || has('win64'))
	" This is for Windows/cygwin and to add -H.
	" '$*' is not passed to the shell, but used by Vim.
	set grepprg=grep\ -nH\ $*\ /dev/null
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

" Completely disable the use of the arrow keys in command and visual modes
no <up> <NOP>
no <down> <NOP>
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


nn ñ :w<CR>
nn <S-ñ> :wq!<CR>

" Plugin keymappings
ino <expr> <tab> emmet#expandAbbrIntelligent("\<tab>")
nn <leader>b :Unite file buffer<CR>
nn <leader>B :Unite -quick-match buffer<CR>

" Stuff to keep in mind:
" ;    Repeat latest f, t, F or T [count] times.
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
set colorcolumn=79
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
set statusline=[%n]\ %f\ %y%<\ %(%M%R%W%)%=[%l,%v]\ (%L,%p%%)
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
if ($TERM == 'xterm-256color' || $TERM == 'screen-256color' || &t_Co >= 256)
	set t_Co=256
endif

set synmaxcol=1024
set background=dark

let g:badwolf_darkgutter=1
let g:badwolf_html_link_underline=0
let g:badwolf_css_props_highlight=1

let g:rehash256=1

let g:gruvbox_italic=0

let g:solarized_termtrans=0
let g:solarized_bold=0
let g:solarized_underline=0
let g:solarized_italic=0

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
        setlocal textwidth=79
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
		au FileType html,css EmmetInstall
		au BufReadPost fugitive://*         set bufhidden=delete
		au FileType json,clojure call FiletypeIndent(0,2)
		au FileType json,clojure RainbowParenthesesToggle
		au FileType json,clojure RainbowParenthesesLoadRound
		au FileType json,clojure RainbowParenthesesLoadSquare
		au FileType json,clojure RainbowParenthesesLoadBraces
		" docpad
		au BufNewFile,BufRead *.cljs        setlocal filetype=clojure
		au BufNewFile,BufRead *.css.scss    setlocal filetype=scss
		au BufNewFile,BufRead *.html.md     setlocal filetype=markdown
		au BufNewFile,BufRead *.html.md.eco setlocal filetype=markdown
		au BufNewFile,BufRead *.html.md     setlocal wrap
		au BufNewFile,BufRead *.html.md.eco setlocal wrap
		
		au FileType text      setlocal formatprg=par\ w79r
		au FileType markdown  setlocal formatprg=par\ w79r
		au FileType gitcommit setlocal formatprg=par\ w72r
		au FileType vim       setlocal foldmethod=marker
	augroup END
endif
" }}}
