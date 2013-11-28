set __fish_git_prompt_show_informative_status 'yes'
set __fish_git_prompt_showdirtystate 'yes'
set __fish_git_prompt_showstashstate 'yes'
set __fish_git_prompt_showupstream 'yes'
set __fish_git_prompt_color_branch yellow

set __fish_git_prompt_char_upstream_ahead 'a'
set __fish_git_prompt_char_upstream_behind 'b'
set __fish_git_prompt_char_dirtystate '+'
set __fish_git_prompt_char_invalidstate 'x'
set __fish_git_prompt_char_stagedstate '*'
set __fish_git_prompt_char_stashstate '$'
set __fish_git_prompt_char_untrackedfiles '.'
set __fish_git_prompt_char_cleanstate 'OK'

set -U EDITOR vim
set -U VISUAL emacs
set -U GIT_EDITOR vim
set -U TERM xterm-256color

set PATH /usr/local/bin /usr/local/sbin /usr/local/share/npm/bin /usr/local/opt/ruby/bin $HOME/bin/cask/bin $HOME/.tmuxifier/tmuxifier/bin $PATH
