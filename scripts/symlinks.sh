#!/usr/bin/zsh
# symlinks.sh by Eduan Lavaque <eduanlavaque@gmail.com>
# Licensed under the MIT license (http://eduan.mit-license.org/)

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
symlink bin            bin
symlink gitconfig      .gitconfig
symlink gitmessage.txt .gitmessage.txt
symlink zsh            .zsh
symlink zshrc          .zshrc
symlink profile        .profile
symlink tmux.conf      .tmux.conf
symlink tmuxifier      .tmuxifier
symlink vim            .vim
symlink vimrc.vim      .vimrc
symlink editorconfig   .editorconfig

# Sublime Text
if [ `uname -s` == 'Linux' ]; then
	ln -fsv $HOME/Dropbox/Sublime\ Text\ 3/Packages $HOME/.config/sublime-text-3/Packages
	ln -fsv $HOME/Dropbox/Sublime\ Text\ 3/Installed\ Packages $HOME/.config/sublime-text-3/Installed\ Packages
elif [ `uname -s` == 'Darwin' ]; then
	ln -fsv $HOME/Dropbox/Sublime\ Text\ 3/Packages $HOME/Library/Application\ Support/Sublime\ Text\ 3/Packages
	ln -fsv $HOME/Dropbox/Sublime\ Text\ 3/Installed\ Packages $HOME/Library/Application\ Support/Sublime\ Text\ 3/Installed\ Packages
fi
