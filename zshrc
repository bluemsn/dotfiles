# Setting my preferred default editor
EDITOR=vim # Command line uses 'vim'
VISUAL=gvim # Visual editor is MacVim
GIT_EDITOR=vim # Editor for Git
export EDITOR VISUAL GIT_EDITOR # Export these options

# Set the config home
export XDG_CONFIG_HOME="${HOME}/.config"

alias ls="ls -la -Gp -F"        # Make 'ls' do lotsa stuff
alias reload!="source ~/.zshrc" # Make 'reload!' source the .zshrc file
alias sudo="sudo "              # Fix using aliases after sudo
alias eduan="cd ~/Desktop/Eduan"

# Initialize colors.
autoload -U colors
colors

# Allow for functions in the prompt.
setopt PROMPT_SUBST

# Load settings for Git prompt
source ~/.zsh/vcs-prompt.sh

# Load Git prompt
source ~/.zsh/zsh-vcs-prompt/zshrc.sh

# Set the prompt
PROMPT=$'%{${fg[green]}%}%n@%m:'  # Green color, current user@server
PROMPT+='%{${fg[blue]}%}%~'       # Blue color, current directory, from $HOME
PROMPT+='%{${fg[default]}%}$(vcs_super_info)' # Default color, Git status
PROMPT+='%{${fg[default]}%} $ ' # Default color, prompt

# Set the right side of my prompt
RPROMPT=$'%T' # Default color, [battery], time

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
export PROMPT_COMMAND="history -a; history -n"

# Autocomplete {{{1
# fixme - the load process here seems a bit bizarre

unsetopt menu_complete   # do not autoselect the first completion entry
unsetopt flowcontrol
setopt auto_menu         # show completion menu on succesive tab press
setopt complete_in_word
setopt always_to_end

WORDCHARS=''

zmodload -i zsh/complist

## case-insensitive (all),partial-word and then substring completion
if [ "x$CASE_SENSITIVE" = "xtrue" ]; then
	zstyle ':completion:*' matcher-list 'r:|[._-]=* r:|=*' 'l:|=* r:|=*'
	unset CASE_SENSITIVE
else
	zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=*' 'l:|=* r:|=*'
fi

zstyle ':completion:*' list-colors ''

# should this be in keybindings?
bindkey -M menuselect '^o' accept-and-infer-next-history

zstyle ':completion:*:*:*:*:*' menu select
zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#) ([0-9a-z-]#)*=01;34=0=01'
zstyle ':completion:*:*:*:*:processes' command "ps -u `whoami` -o pid,user,comm -w -w"

# disable named-directories autocompletion
zstyle ':completion:*:cd:*' tag-order local-directories directory-stack path-directories
cdpath=(.)

# use /etc/hosts and known_hosts for hostname completion
[ -r /etc/ssh/ssh_known_hosts ] && _global_ssh_hosts=(${${${${(f)"$(</etc/ssh/ssh_known_hosts)"}:#[\|]*}%%\ *}%%,*}) || _global_ssh_hosts=()
[ -r ~/.ssh/known_hosts ] && _ssh_hosts=(${${${${(f)"$(<$HOME/.ssh/known_hosts)"}:#[\|]*}%%\ *}%%,*}) || _ssh_hosts=()
[ -r ~/.ssh/config ] && _ssh_config=($(cat ~/.ssh/config | sed -ne 's/Host[=\t ]//p')) || _ssh_config=()
[ -r /etc/hosts ] && : ${(A)_etc_hosts:=${(s: :)${(ps:\t:)${${(f)~~"$(</etc/hosts)"}%%\#*}##[:blank:]#[^[:blank:]]#}}} || _etc_hosts=()
hosts=(
	"$_ssh_config[@]"
	"$_global_ssh_hosts[@]"
	"$_ssh_hosts[@]"
	"$_etc_hosts[@]"
	"$HOST"
	localhost
)
zstyle ':completion:*:hosts' hosts $hosts
zstyle ':completion:*' users off

# Use caching so that commands like apt and dpkg complete are useable
zstyle ':completion::complete:*' use-cache 1
zstyle ':completion::complete:*' cache-path $ZSH/cache/

# Don't complete uninteresting users
zstyle ':completion:*:*:*:users' ignored-patterns \
	adm amanda apache avahi beaglidx bin cacti canna clamav daemon \
	dbus distcache dovecot fax ftp games gdm gkrellmd gopher \
	hacluster haldaemon halt hsqldb ident junkbust ldap lp mail \
	mailman mailnull mldonkey mysql nagios \
	named netdump news nfsnobody nobody nscd ntp nut nx openvpn \
	operator pcap postfix postgres privoxy pulse pvm quagga radvd \
	rpc rpcuser rpm shutdown squid sshd sync uucp vcsa xfs

# ... unless we really want to.
zstyle '*' single-ignored show

if [ "x$COMPLETION_WAITING_DOTS" = "xtrue" ]; then
	expand-or-complete-with-dots() {
		echo -n "\e[31m......\e[0m"
		zle expand-or-complete
		zle redisplay
	}
	zle -N expand-or-complete-with-dots
	bindkey "^I" expand-or-complete-with-dots
	fi
# }}}1

# Fix some keys in Linux
#if [ `uname -s` == 'Linux' ]; then
#	bindkey "\e[1~" beginning-of-line
#	bindkey "\e[4~" end-of-line
#	bindkey "\e[5~" beginning-of-history
#	bindkey "\e[6~" end-of-history
#	bindkey "\e[7~" beginning-of-line
#	bindkey "\e[3~" delete-char
#	bindkey "\e[2~" quoted-insert
#	bindkey "\e[5C" forward-word
#	bindkey "\e[5D" backward-word
#	bindkey "\e\e[C" forward-word
#	bindkey "\e\e[D" backward-word
#	bindkey "\e[1;5C" forward-word
#	bindkey "\e[1;5D" backward-word
#	bindkey "\e[8~" end-of-line
#	bindkey "\eOH" beginning-of-line
#	bindkey "\eOF" end-of-line
#	bindkey "\e[H" beginning-of-line
#	bindkey "\e[F" end-of-line
#fi

# vim: set foldmarker={{{,}}}
