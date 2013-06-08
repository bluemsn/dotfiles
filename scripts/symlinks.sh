#!/usr/bin/zsh
# symlinks.sh by Eduan Lavaque <eduan@snapsimpletech.com>
# Licensed under the MIT license (http://eduan.mit-license.org/)
# This file will generate my symlinks. For new installs.

ln -fs $HOME/dotfiles/linux/openbox            $HOME/.config/openbox
ln -fs $HOME/dotfiles/linux/themes             $HOME/.themes
ln -fs $HOME/dotfiles/linux/conkyrc.txt        $HOME/.conkyrc
ln -fs $HOME/dotfiles/linux/tint2rc.txt        $HOME/.config/tint2/tint2rc
ln -fs $HOME/dotfiles/linux/xbindkeysrc.txt    $HOME/.xbindkeysrc
ln -fs $HOME/dotfiles/linux/xinitrc.txt        $HOME/.xinitrc
ln -fs $HOME/dotfiles/random/gitmessage.txt    $HOME/.gitmessage.txt
ln -fs $HOME/dotfiles/shells/bash              $HOME/.bash
ln -fs $HOME/dotfiles/shells/bash/bash_profile $HOME/.bash_profile
ln -fs $HOME/dotfiles/shells/bash/bashrc       $HOME/.bashrc
ln -fs $HOME/dotfiles/shells/zsh               $HOME/.zsh
ln -fs $HOME/dotfiles/shells/zsh/zshrc         $HOME/.zshrc
ln -fs $HOME/dotfiles/profile                  $HOME/.profile
ln -fs $HOME/dotfiles/ssh                      $HOME/.ssh
ln -fs $HOME/dotfiles/tmux/tmux.conf           $HOME/.tmux.conf
ln -fs $HOME/dotfiles/vim                      $HOME/.vim
ln -fs $HOME/dotfiles/vim/gvimrc.vim           $HOME/.gvimrc
ln -fs $HOME/dotfiles/vim/vimrc.vim            $HOME/.vimrc

rm -r $HOME/.config/sublime-text-2/Installed\ Packages
rm -r $HOME/.config/sublime-text-2/Packages
rm -r $HOME/.config/sublime-text-2/Pristine\ Packages
ln -s $HOME/Dropbox/Sublime\ Text\ 2/Installed\ Packages $HOME/.config/sublime-text-2/Installed\ Packages
ln -s $HOME/Dropbox/Sublime\ Text\ 2/Packages            $HOME/.config/sublime-text-2/Packages
ln -s $HOME/Dropbox/Sublime\ Text\ 2/Pristine\ Packages  $HOME/.config/sublime-text-2/Pristine\ Packages

sudo ln -fs $HOME/dotfiles/bin/tmux-vim-select-pane /usr/local/bin/tmux-vim-select-pane
sudo ln -fs $HOME/dotfiles/bin/feh_browser.sh       /usr/local/bin/feh_browser
