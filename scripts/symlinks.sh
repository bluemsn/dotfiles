#!/usr/bin/zsh
# symlinks.sh by Eduan Lavaque <eduan@snapsimpletech.com>
# Licensed under the MIT license (http://eduan.mit-license.org/)
# This file will generate my symlinks. For new installs.

symlink() {
	ln -fsv $HOME/dotfiles/$1 $HOME/$2
}

symlink config/alopex      .config/alopex
symlink conkyrc.txt        .conkyrc
mkdir $HOME/.config/tint2
symlink config/tint2rc.txt .config/tint2/tint2rc
symlink xbindkeysrc.txt    .xbindkeysrc
symlink xinitrc.txt        .xinitrc
symlink gitmessage.txt     .gitmessage.txt
symlink zsh                .zsh
symlink zshrc              .zshrc
symlink profile            .profile
symlink tmux.conf          .tmux.conf
symlink vim                .vim
symlink gvimrc.vim         .gvimrc
symlink vimrc.vim          .vimrc

# Sublime Text 2
ln -fsv $HOME/Dropbox/Sublime\ Text\ 2/Installed\ Packages $HOME/.config/sublime-text-2/Installed\ Packages
ln -fsv $HOME/Dropbox/Sublime\ Text\ 2/Packages            $HOME/.config/sublime-text-2/Packages
ln -fsv $HOME/Dropbox/Sublime\ Text\ 2/Pristine\ Packages  $HOME/.config/sublime-text-2/Pristine\ Packages
# Sublime Text 3
ln -fsv $HOME/Dropbox/Sublime\ Text\ 3/Cache               $HOME/.config/sublime-text-3/Cache
ln -fsv $HOME/Dropbox/Sublime\ Text\ 3/Installed\ Packages $HOME/.config/sublime-text-3/Installed\ Packages
ln -fsv $HOME/Dropbox/Sublime\ Text\ 3/Local               $HOME/.config/sublime-text-3/Local
ln -fsv $HOME/Dropbox/Sublime\ Text\ 3/Packages            $HOME/.config/sublime-text-3/Packages

sudo ln -fsv $HOME/dotfiles/bin/tmux-vim-select-pane /usr/local/bin/tmux-vim-select-pane
#sudo ln -fs $HOME/dotfiles/bin/feh_browser.sh       /usr/local/bin/feh_browser
