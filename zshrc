EDITOR=vim
VISUAL=subl3
GIT_EDITOR=vim
PAGER=most
export EDITOR VISUAL GIT_EDITOR PAGER

export XDG_CONFIG_HOME="$HOME/.config"
export PATH="/usr/local/bin:/usr/local/sbin:/usr/local/share/npm/bin:/usr/local/opt/ruby/bin:$HOME/.emacs.d/cask/bin:$PATH"

# Aliases
alias ls="ls -la -Gp -F"
alias sudo="sudo "
alias tree='tree -CAFa -I "CVS|*.*.package|.svn|.git|node_modules" --dirsfirst'
alias tmux='tmux -2'
alias perm='stat -c "%A  %a  %U:%G  $(pwd)/%n" .* *'
alias t='python2 ~/bin/t/t.py --task-dir ~/tasks --list tasks.txt'
alias z='. ~/bin/z/z.sh'

source ~/.zsh/prompt.sh

# History
HISTFILE="$HOME/.zhistory"
HISTSIZE=10000000
SAVEHIST=10000000

setopt BANG_HIST
setopt EXTENDED_HISTORY
setopt INC_APPEND_HISTORY
setopt SHARE_HISTORY
setopt HIST_EXPIRE_DUPS_FIRST
setopt HIST_IGNORE_DUPS
setopt HIST_IGNORE_ALL_DUPS
setopt HIST_FIND_NO_DUPS
setopt HIST_IGNORE_SPACE
setopt HIST_SAVE_NO_DUPS
setopt HIST_REDUCE_BLANKS
setopt HIST_VERIFY
setopt HIST_BEEP

# Share history between tmux sessions
# http://stackoverflow.com/q/12247777/1622940
# export PROMPT_COMMAND="history -a; history -n"

# Fix some keys in Linux, thanks Daniel!
#bindkey "\e[1~" beginning-of-line
#bindkey "\e[4~" end-of-line
#bindkey "\e[5~" beginning-of-history
#bindkey "\e[6~" end-of-history
#bindkey "\e[7~" beginning-of-line
#bindkey "\e[3~" delete-char
#bindkey "\e[2~" quoted-insert
#bindkey "\e[5C" forward-word
#bindkey "\e[5D" backward-word
#bindkey "\e\e[C" forward-word
#bindkey "\e\e[D" backward-word
#bindkey "\e[1;5C" forward-word
#bindkey "\e[1;5D" backward-word
#bindkey "\e[8~" end-of-line
#bindkey "\eOH" beginning-of-line
#bindkey "\eOF" end-of-line
#bindkey "\e[H" beginning-of-line
#bindkey "\e[F" end-of-line

function gif-ify() {
	if [[ -n "$1" && -n "$2" ]]; then
		ffmpeg -i $1 -pix_fmt rgb24 temp.gif
		convert -layers Optimize temp.gif $2
		rm temp.gif
	else
		echo "proper usage: gif-ify <input_movie.mov> <output_file.gif>. You DO need to include extensions."
	fi
}
function gi() { curl http://gitignore.io/api/$1 ;}

# vim: set foldmarker={{{,}}}
