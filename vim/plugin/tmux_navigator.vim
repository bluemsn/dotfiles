" Maps <C-h/j/k/l> to switch Vim splits in the given direction. If there are
" no more windows in that direction, forwards the operation to tmux.
" Aditionally, <C-\> toggles between last active Vim splits/tmux panes.

if ($TMUX != '')
	let s:tmux_is_last_pane = 0
	au WinEnter * let s:tmux_is_last_pane = 0

	" Like `wincmd` but also change tmux panes instead of Vim windows when
	" needed.
	function s:TmuxWinCmd(direction)
		let nr = winnr()
		let tmux_last_pane = (a:direction == 'p' && s:tmux_is_last_pane)
		if (!tmux_last_pane)
			" Try to switch windows within Vim.
			exec 'wincmd ' . a:direction
		endif
		" Forward the switch panes command to tmux if:
		" a) we're toggling between the last tmux pane;
		" b) we tried switching windows in Vim but it didn't have effect.
		if (tmux_last_pane || nr == winnr())
			let cmd = 'tmux select-pane -' . tr(a:direction, 'phjkl', 'lLDUR')
			call system(cmd)
			let s:tmux_is_last_pane = 1
			echo cmd
		else
			let s:tmux_is_last_pane = 0
		endif
	endfunction

	" Navigate between split windows/tmux panes.
	nnoremap <C-j> :call <SID>TmuxWinCmd('j')<CR>
	nnoremap <C-k> :call <SID>TmuxWinCmd('k')<CR>
	nnoremap <C-h> :call <SID>TmuxWinCmd('h')<CR>
	nnoremap <C-l> :call <SID>TmuxWinCmd('l')<CR>
	nnoremap <C-\> :call <SID>TmuxWinCmd('p')<CR>
endif
