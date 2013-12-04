#!/usr/bash
# symlinks.sh by Eduan Lavaque <eduanlavaque@gmail.com>
# Licensed under the MIT license (http://eduan.mit-license.org/)

symlink() {
	ln -fsv $HOME/dotfiles/$1 $HOME/$2
}

mkdir -p $HOME/.config

# dotfiles symlinks
if [ `uname -s` == 'Linux' ]; then
	symlink config/alopex      .config/alopex
	mkdir -p $HOME/.config/tint2
	symlink config/tint2rc     .config/tint2/tint2rc
	symlink conkyrc            .conkyrc
	symlink curlrc             .curlrc
	symlink xbindkeysrc        .xbindkeysrc
	symlink xinitrc            .xinitrc
fi
symlink bin            bin
symlink emacs.d        .emacs.d
symlink gitconfig      .gitconfig
symlink inputrc        .inputrc
symlink irssi          .irssi
symlink zsh            .zsh
symlink zshrc          .zshrc
symlink profile        .profile
symlink tmux.conf      .tmux.conf
symlink vim            .vim
symlink vimrc          .vimrc
symlink editorconfig   .editorconfig
symlink config/fish    .config/fish

# Sublime Text
if [ `uname -s` == 'Linux' ]; then
	ln -fsv $HOME/Dropbox/Sublime\ Text\ 3/Packages $HOME/.config/sublime-text-3/Packages
	ln -fsv $HOME/Dropbox/Sublime\ Text\ 3/Installed\ Packages $HOME/.config/sublime-text-3/Installed\ Packages
elif [ `uname -s` == 'Darwin' ]; then
	ln -fsv $HOME/Dropbox/Sublime\ Text\ 3/Packages $HOME/Library/Application\ Support/Sublime\ Text\ 3/Packages
	ln -fsv $HOME/Dropbox/Sublime\ Text\ 3/Installed\ Packages $HOME/Library/Application\ Support/Sublime\ Text\ 3/Installed\ Packages
fi
