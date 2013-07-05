#!/usr/bin/zsh
# symlinks.sh by Eduan Lavaque <eduan@snapsimpletech.com>
# Licensed under the MIT license (http://eduan.mit-license.org/)
# This file will generate my symlinks. For new installs.

symlink() {
	ln -fsv $HOME/dotfiles/$1 $HOME/$2
}

symlink linux/alopex             .config/alopex
#symlink linux/openbox            .config/openbox
#symlink linux/themes             .themes
symlink linux/conkyrc.txt        .conkyrc
mkdir $HOME/.config/tint2
symlink linux/tint2rc.txt        .config/tint2/tint2rc
symlink linux/xbindkeysrc.txt    .xbindkeysrc
symlink linux/xinitrc.txt        .xinitrc
symlink random/gitmessage.txt    .gitmessage.txt
symlink shells/bash              .bash
symlink shells/bash/bash_profile .bash_profile
symlink shells/bash/bashrc       .bashrc
symlink shells/zsh               .zsh
symlink shells/zsh/zshrc         .zshrc
symlink shells/profile           .profile
symlink ssh                      .ssh
symlink tmux/tmux.conf           .tmux.conf
symlink vim                      .vim
symlink vim/gvimrc.vim           .gvimrc
symlink vim/vimrc.vim            .vimrc

# Sublime Text 2
rm -r $HOME/.config/sublime-text-2/Installed\ Packages
rm -r $HOME/.config/sublime-text-2/Packages
rm -r $HOME/.config/sublime-text-2/Pristine\ Packages
ln -s $HOME/Dropbox/Sublime\ Text\ 2/Installed\ Packages $HOME/.config/sublime-text-2/Installed\ Packages
ln -s $HOME/Dropbox/Sublime\ Text\ 2/Packages            $HOME/.config/sublime-text-2/Packages
ln -s $HOME/Dropbox/Sublime\ Text\ 2/Pristine\ Packages  $HOME/.config/sublime-text-2/Pristine\ Packages
# Sublime Text 3
rm -r $HOME/.config/sublime-text-3/Cache
rm -r $HOME/.config/sublime-text-3/Installed\ Packages
rm -r $HOME/.config/sublime-text-3/Local
rm -r $HOME/.config/sublime-text-3/Packages
ln -s $HOME/Dropbox/Sublime Text\ 3/Cache               $HOME/.config/sublime-text-3/Cache
ln -s $HOME/Dropbox/Sublime Text\ 3/Installed\ Packages $HOME/.config/sublime-text-3/Installed\ Packages
ln -s $HOME/Dropbox/Sublime Text\ 3/Local               $HOME/.config/sublime-text-3/Local
ln -s $HOME/Dropbox/Sublime Text\ 3/Packages            $HOME/.config/sublime-text-3/Packages

sudo ln -fs $HOME/dotfiles/bin/tmux-vim-select-pane /usr/local/bin/tmux-vim-select-pane
#sudo ln -fs $HOME/dotfiles/bin/feh_browser.sh       /usr/local/bin/feh_browser
