#!/bin/zsh
# =============================================================================
# make_symlinks.sh
# -----------------------------------------------------------------------------
# This file automatically makes all your defined symlinks, in one go. And
# will also delete them with another script, if you say yes, and you have it.
# =============================================================================
# Eduan Lavaque <eduan@snapsimpletech.com>
# Licensed under the MIT license (htpp://eduan.mit-license.org/2012-2013)
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

# Recognize the dotfiles directory
dotfiles_dir="$script_dir/.."

echo "Started making new symlinks, at [$time]...\n"

if [ $kernel == 'Darwin' ]; then
	# Making symlinks to Vim files, add yours as you need
	echo "Making symlinks to Vim files"
	ln -sfn -v $dotfiles_dir/vim ~/.vim
	ln -sfn -v $dotfiles_dir/vim/vimrc.vim ~/.vimrc
	ln -sfn -v $dotfiels_dir/vim/gvimrc.vim ~/.gvimrc
	echo "Done at [$time]...\n"

	# Making symlinks to Tmux files, add yours as you need
	echo "Making symlinks to Tmux files"
	ln -sfn -v $dotfiles_dir/tmux/tmux.conf ~/.tmux.conf
	echo "Done at  [$time]...\n"

	# Making symlinks to Zsh files, add yours as you need
	echo 'Making symlinks to Zsh files'
	ln -sfn -v $dotfiles_dir/shells/zsh ~/.zsh
	ln -sfn -v $dotfiles_dir/shells/zsh/zshrc ~/.zshrc
	ln -sfn -v $dotfiles_dir/shells/profile ~/.profile
	echo "Done at [$time]...\n"

	# Making symlinks to Git files, add yours as you need
	echo 'Making symlinks to Git files'
	ln -sfn -v $dotfiles_dir/git/gitconfig ~/.gitconfig
	ln -sfn -v $dotfiles_dir/git/gitmessage.txt ~/.gitmessage.txt
	echo "Done at [$time]...\n"
fi

if [ $kernel == 'Linux' ]; then
	# Making symlinks to Vim files, add yours as you need
	echo 'Making symlinks to Vim files'
	ln -sfn -v $dotfiles_dir/vim /home/$user/.vim
	ln -sfn -v $dotfiles_dir/vim/vimrc /home/$user/.vimrc
	ln -sfn -v $dotfiles_dir/vim/gvimrc /home/$user/.gvimrc
	echo "Done at [$time]...\n"

	# Making symlinks to Tmux files, add yours as you need
	echo "Making symlinks to Tmux files"
	ln -sfn -v $dotfiles_dir/vim/tmux.conf /home/$user/.tmux.conf
	echo "Done at  [$time]...\n"

	# Making symlinks to Zsh files, add yours as you need
	echo 'Making symlinks to Zsh files'
	ln -sfn -v $dotfiles_dir/zsh /home/$user/.zsh
	ln -sfn -v $dotfiles_dir/zsh/zshrc /home/$user/.zshrc
	ln -sfn -v $dotfiles_dir/shells/profile ~/.profile
	echo "Done at [$time]...\n"

	# Making symlinks to Git files, add yours as you need
	echo 'Making symlinks to Git files'
	ln -sfn -v $dotfiles_dir/git/gitconfig /home/$user/.gitconfig
	ln -sfn -v $dotfiles_dir/git/gitmessage.txt /home/$user/.gitmessage.txt
	echo "Done at [$time]...\n"
fi

echo "Finished making new symlinks, at [$time]..."
