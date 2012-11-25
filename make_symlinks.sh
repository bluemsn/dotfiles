#!/bin/zsh
# =============================================================================
# make_symlinks.sh
# -----------------------------------------------------------------------------
# This file automatically makes all your defined symlinks, in one go. And
# will also delete them with another script, if you say yes, and you have it.
# =============================================================================
# Eduan Lavaque <eduan@snapsimpletech.com>
# Licensed under the MIT license (htpp://eduan.mit-license.org/)
# =============================================================================

kernel=`uname -s`        # Current Kernel name
user=`whoami`            # Current Unix username
time=`date +%H:%M`       # Output current time
current_dir=$(pwd)       # Shell's current location
script_dir=$(dirname $0) # This script's current location

# This if statement is to know the correct location of the script, if the
# shell's location is the same as the script's location
if [ $script_dir == '.' ]; then
	script_dir="$current_dir"
fi

read -p 'Do you wish to delete the old symlinks first? [y/n] ' prompt1

case "$prompt1" in
	y|Y ) sh ./delete_symlinks.sh;;
	n|N ) exit;;
	* ) echo 'Invalid answer!';;
esac

echo "Started making new symlinks, at [$time]..."

if [ $kernel == 'Darwin' ]; then
	# Making symlinks to Vim files, add yours as you need
	echo "\nMaking symlinks to Vim files"
	ln -s -v $script_dir/vim ~/.vim
	ln -s -v $script_dir/vim/vimrc ~/.vimrc
	ln -s -v $script_dir/vim/gvimrc ~/.gvimrc
	echo "Done at [$time]...\n"
	
	# Making symlinks to Zsh files, add yours as you need
	echo 'Making symlinks to Zsh files'
	ln -s -v $script_dir/zsh ~/.zsh
	ln -s -v $script_dir/zsh/zshrc ~/.zshrc
	echo "Done at [$time]...\n"

	# Making symlinks to Git files, add yours as you need
	echo 'Making symlinks to Git files'
	ln -s -v $script_dir/git/gitconfig ~/.gitconfig
	ln -s -v $script_dir/git/gitmessage.txt ~/.gitmessage.txt
	echo "Done at [$time]...\n"
fi

if [ $kernel == 'Linux' ]; then
	# Making symlinks to Vim files, add yours as you need
	echo 'Making symlinks to Vim files'
	ln -s -v $script_dir/vim /home/$user/.vim
	ln -s -v $script_dir/vim/vimrc /home/$user/.vimrc
	ln -s -v $script_dir/vim/gvimrc /home/$user/.gvimrc
	echo "Done at [$time]...\n"

	# Making symlinks to Zsh files, add yours as you need
	echo 'Making symlinks to Zsh files'
	ln -s -v $script_dir/zsh /home/$user/.zsh
	ln -s -v $script_dir/zsh/zshrc /home/$user/.zshrc
	echo "Done at [$time]...\n"

	# Making symlinks to Git files, add yours as you need
	echo 'Making symlinks to Git files'
	ln -s -v $script_dir/git/gitconfig /home/$user/.gitconfig
	ln -s -v $script_dir/git/gitmessage.txt /home/$user/.gitmessage.txt
	echo "Done at [$time]...\n"
fi

echo "Finished making new symlinks, at [$time]..."
