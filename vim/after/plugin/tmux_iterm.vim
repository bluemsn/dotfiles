" This file contains some stuff in order for Vim to work correctly with an
" environment that's using Tmux or iTerm.
"
" This file was grabbed directly from here:
" https://github.com/aaronjensen/vimfiles/blob/d6963278c57d3cecfaee09c2c05ebb8e245a2673/vimrc#L460-L516
"
" However it might contain some changes, mostly to suit my tastes. Not saying
" it has though.

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" This enables iterm cursor changes from vim. In .tmux.conf you'll need:
" set-option -g terminal-overrides '*88col*:colors=88,*256col*:colors=256,xterm*:XT:Ms=\E]52;%p1%s;%p2%s\007:Cc=\E]12;%p1%s\007:Cr=\E]112\007:Cs=\E]50;CursorShape=%?%p1%{3}%<%t%{0}%e%p1%{2}%-%;%d\007'
"
" Source: https://github.com/Casecommons/casecommons_workstation/blob/master/templates/default/dot_tmux.conf.erb
"         https://github.com/Casecommons/vim-config/blob/master/init/tmux.vim
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
if exists('$ITERM_PROFILE')
	if exists('$TMUX')
		let &t_SI = "\<Esc>[3 q"
		let &t_EI = "\<Esc>[0 q"
	else
		let &t_SI = "\<Esc>]50;CursorShape=1\x7"
		let &t_EI = "\<Esc>]50;CursorShape=0\x7"
	endif
end

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" This fixes pasting from iterm (and some other terminals, but you'll need to
" adjust the condition) by using "bracketed paste mode"
" I modified it to work in tmux and not wait for esc (by using f28/f29)
"
" See: http://stackoverflow.com/questions/5585129/pasting-code-into-terminal-window-into-vim-on-mac-os-x
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Tmux wrapping borrowed from vitality.vim: https://github.com/sjl/vitality.vim
function WrapForTmux(s)
	if !exists('$TMUX')
		return a:s
	endif

	let tmux_start = "\<Esc>Ptmux;"
	let tmux_end = "\<Esc>\\"

	return tmux_start . substitute(a:s, "\<Esc>", "\<Esc>\<Esc>", 'g') . tmux_end
endfunction

if exists('$ITERM_PROFILE')
	" I'm just setting bracketed paste mode in my bashrc now. Setting and
	" unsetting doesn't work very well with tmux as it affects other shells.
	" put this in your bashrc: [ -n "$ITERM_PROFILE" ] && printf "\e[?2004h"
	"
	" let &t_ti = WrapForTmux("\<Esc>[?2004h") . &t_ti
	" let &t_te = WrapForTmux("\<Esc>[?2004l") . &t_te
	function XTermPasteBegin(ret)
		set pastetoggle=<Esc>[201~
		set paste
		return a:ret
	endfunction

	execute "set <f28>=\<Esc>[200~"
	execute "set <f29>=\<Esc>[201~"
	map <expr> <f28> XTermPasteBegin("i")
	imap <expr> <f28> XTermPasteBegin("")
	vmap <expr> <f28> XTermPasteBegin("c")
	cmap <f28> <nop>
	cmap <f29> <nop>
end

" vim: set nowrap fdm={{{,}}}
