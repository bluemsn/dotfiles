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
RPROMPT=$'$(acpi | grep -o \'[0-9]*%\')% %T' # Default color, batter, time

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

# vim: set foldmarker={{{,}}}
