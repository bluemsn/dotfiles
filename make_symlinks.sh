#####################
# make_symlinks.sh
# This file automatically makes all your defined symlinks, in one go.
# This file must be at the root of your dotfiles folder.
#####################
# Copyright (C) 2012 Eduan Lavaque <eduan@snapsimpletech.com>
# Licenced under the MIT license (http://mit-license.org/)
#####################

current_dir=$(pwd)       # Shell's current location
script_dir=$(dirname $0) # This script's current location

# This if statement is to know the correct location of the script, if the
# shell's location is the same as the script's location
if [ $script_dir = '.' ]; then
	script_dir="$current_dir"
fi

# Making symlinks to Vim files, add yours as you need
echo 'Making symlinks to Vim files'
ln -s -v $script_dir/vim ~/.vim
ln -s -v $script_dir/vim/vimrc ~/.vimrc
ln -s -v $script_dir/vim/gvimrc ~/.gvimrc
echo "Done...\n"

# Making symlinks to Zsh files, add yours as you need
echo 'Making symlinks to Zsh files'
ln -s -v $script_dir/zsh ~/.zsh
ln -s -v $script_dir/zsh/zshrc ~/.zshrc
echo "Done...\n"

# Making symlinks to Git files, add yours as you need
echo 'Making symlinks to Git files'
ln -s -v $script_dir/git/gitconfig ~/.gitconfig
ln -s -v $script_dir/git/gitmessage.txt ~/.gitmessage.txt
echo "Done...\n"
