#!/usr/bin/zsh
# symlinks.sh by Eduan Lavaque <eduan@snapsimpletech.com>
# Licensed under the MIT license (http://eduan.mit-license.org/)
# This file will generate my symlinks. For new installs.

symlink() {
	ln -fsv $HOME/dotfiles/$1 $HOME/$2
}

# dotfiles symlinks
if [ `uname -s` == 'Linux' ]; then
	symlink config/alopex      .config/alopex
	mkdir $HOME/.config/tint2
	symlink config/tint2rc.txt .config/tint2/tint2rc
	symlink conkyrc.txt        .conkyrc
	symlink curlrc             .curlrc
	symlink xbindkeysrc.txt    .xbindkeysrc
	symlink xinitrc.txt        .xinitrc
fi
symlink gitmessage.txt     .gitmessage.txt
symlink zsh                .zsh
symlink zshrc              .zshrc
symlink profile            .profile
symlink tmux.conf          .tmux.conf
symlink vim                .vim
symlink gvimrc.vim         .gvimrc
symlink vimrc.vim          .vimrc

# Sublime Text
if [ `uname -s` == 'Linux' ]; then
	ln -fsv $HOME/Dropbox/Sublime\ Text\ 3 $HOME/.config/sublime-text-3
elif [ `uname -s` == 'Darwin' ]; then
	ln -fsv $HOME/Dropbox/Sublime\ Text\ 3 $HOME/Library/Application\ Support/Sublime\ Text\ 3
fi

# My own commands
if [ `uname -s` == 'Linux' ]; then
	sudo ln -fsv $HOME/dotfiles/bin/tmux-vim-select-pane /usr/local/bin/tmux-vim-select-pane
	#sudo ln -fsv $HOME/dotfiles/bin/feh_browser.sh       /usr/local/bin/feh_browser
elif [ `uname -s` == 'Darwin' ]; then
	# Gotta do stuff here...
fi
