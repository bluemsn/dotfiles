EDITOR=vim
VISUAL=vim
GIT_EDITOR=vim
export EDITOR VISUAL GIT_EDITOR
export TERM=rxvt-unicode
source ~/.vim/bundle/gruvbox/gruvbox_256palette.sh

export XDG_CONFIG_HOME="${HOME}/.config"
export PATH="/usr/local/bin:/usr/local/sbin:/usr/local/share/npm/bin:/usr/local/opt/ruby/bin:$HOME/bin/cask/bin:$PATH"

alias ls="ls -la -Gp -F"
alias sudo="sudo "
alias t='python ~/bin/t/t.py --task-dir ~/tasks --list tasks'
alias tree='tree -CAFa -I "CVS|*.*.package|.svn|.git|node_modules" --dirsfirst'
alias tmux='tmux -2'

autoload -U colors
colors
setopt PROMPT_SUBST
source ~/.zsh/safe-paste.sh
source ~/.zsh/vcs-prompt.sh
source ~/.zsh/zsh-vcs-prompt/zshrc.sh

#                          user                @                server            path                git info                             prompt
PROMPT=$'%{${fg[green]}%}%n%{${fg[default]}%}@%{${fg[green]}%}%m %{${fg[blue]}%}%~%{${fg[default]}%}$(vcs_super_info)%{${fg[default]}%} %{${fg[red]}%}$%{${fg[default]}%} '
#         time              last exit status
RPROMPT=$'%T %{${fg[red]}%}[%?]'

# Sets history options.
#
# Authors:
#   Robby Russell <robby@planetargon.com>
#   Sorin Ionescu <sorin.ionescu@gmail.com>
#

HISTFILE="$HOME/.zhistory"
HISTSIZE=10000000
SAVEHIST=10000000

setopt BANG_HIST              # Treat the '!' character specially during expansion.
setopt EXTENDED_HISTORY       # Write the history file in the ":start:elapsed;command" format.
setopt INC_APPEND_HISTORY     # Write to the history file immediately, not when the shell exits.
setopt SHARE_HISTORY          # Share history between all sessions.
setopt HIST_EXPIRE_DUPS_FIRST # Expire duplicate entries first when trimming history.
setopt HIST_IGNORE_DUPS       # Don't record an entry that was just recorded again.
setopt HIST_IGNORE_ALL_DUPS   # Delete old recorded entry if new entry is a duplicate.
setopt HIST_FIND_NO_DUPS      # Do not display a line previously found.
setopt HIST_IGNORE_SPACE      # Don't record an entry starting with a space.
setopt HIST_SAVE_NO_DUPS      # Don't write duplicate entries in the history file.
setopt HIST_REDUCE_BLANKS     # Remove superfluous blanks before recording entry.
setopt HIST_VERIFY            # Don't execute immediately upon history expansion.
setopt HIST_BEEP              # Beep when accessing nonexistent history.

# Share history between tmux sessions
# http://stackoverflow.com/q/12247777/1622940
# export PROMPT_COMMAND="history -a; history -n"

# Fix some keys in Linux
bindkey "\e[1~" beginning-of-line
bindkey "\e[4~" end-of-line
bindkey "\e[5~" beginning-of-history
bindkey "\e[6~" end-of-history
bindkey "\e[7~" beginning-of-line
bindkey "\e[3~" delete-char
bindkey "\e[2~" quoted-insert
bindkey "\e[5C" forward-word
bindkey "\e[5D" backward-word
bindkey "\e\e[C" forward-word
bindkey "\e\e[D" backward-word
bindkey "\e[1;5C" forward-word
bindkey "\e[1;5D" backward-word
bindkey "\e[8~" end-of-line
bindkey "\eOH" beginning-of-line
bindkey "\eOF" end-of-line
bindkey "\e[H" beginning-of-line
bindkey "\e[F" end-of-line

source ~/.zsh/fish-highlighting/zsh-syntax-highlighting.zsh

# vim: set foldmarker={{{,}}}
