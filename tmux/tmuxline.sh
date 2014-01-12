# This tmux statusbar config was created by tmuxline.vim
# on Sun, 12 Jan 2014

set -g status-bg 'colour237'
set -g message-command-fg 'colour248'
set -g status-justify 'left'
set -g status-left-length '100'
set -g status 'on'
set -g pane-active-border-fg 'colour142'
set -g message-bg 'colour241'
set -g status-right-length '100'
set -g status-right-attr 'none'
set -g message-fg 'colour248'
set -g message-command-bg 'colour241'
set -g status-attr 'none'
set -g status-utf8 'on'
set -g pane-border-fg 'colour241'
set -g status-left-attr 'none'
setw -g window-status-fg 'colour243'
setw -g window-status-attr 'none'
setw -g window-status-activity-bg 'colour237'
setw -g window-status-activity-attr 'none'
setw -g window-status-activity-fg 'colour142'
setw -g window-status-separator ''
setw -g window-status-bg 'colour237'
set -g status-left '#[fg=colour235,bg=colour142] #S #[fg=colour142,bg=colour237,nobold,nounderscore,noitalics]'
set -g status-right '#[fg=colour241,bg=colour237,nobold,nounderscore,noitalics]#[fg=colour248,bg=colour241] %Y-%m-%d  %H:%M #[fg=colour142,bg=colour241,nobold,nounderscore,noitalics]#[fg=colour235,bg=colour142] #h '
setw -g window-status-format '#[fg=colour243,bg=colour237] #I #[fg=colour243,bg=colour237] #W '
setw -g window-status-current-format '#[fg=colour237,bg=colour241,nobold,nounderscore,noitalics]#[fg=colour248,bg=colour241] #I #[fg=colour248,bg=colour241] #W #[fg=colour241,bg=colour237,nobold,nounderscore,noitalics]'
