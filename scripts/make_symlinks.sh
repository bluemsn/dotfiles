#!/bin/zsh
# =============================================================================
# make_symlinks.sh
# -----------------------------------------------------------------------------
# This file automatically makes all your defined symlinks, in one go. And will
# also delete the symlinks if they existed before.
# =============================================================================
# Eduan Lavaque <eduan@snapsimpletech.com>
# Licensed under the MIT license (http://eduan.mit-license.org/2012-2013)
# =============================================================================

kernel=`uname -s`        # Current Kernel name
user=`whoami`            # Current Unix username
time=`date +%H:%M`       # Output current time
current_dir=$(pwd)       # Shell's current location
script_dir=$(dirname $0) # This script's current location

if [ $kernel == 'Darwin' ]
	# Do nothing, home is already set
elif [ $kernel == 'Linux' ]
	$HOME="/home/$user"
fi

# This if statement is to know the correct location of the script, if the
# shell's location is the same as the script's location
if [ $script_dir == '.' ]; then
	script_dir="$current_dir"
fi

# Let the script know what the dotfiles dir is
dotfiles_dir="$HOME/dotfiles"

# Exit the script if I'm not sure of what Unix kernel this is being run on
if [ $kernel != 'Darwin' || $kernel != 'Linux' ]; then
	echo "I do not recognize this Unix kernel."
	exit
fi

echo "Started making new symlinks, at [$time]...\n"
echo "First I'm gonna backup any of the files that I find, just in case..."

if [ -d "$HOME/.dotfiles_old" ]; then
	if [ $kernel == 'Darwin' ]; then
		echo "\nMaking backup directory..."
		mkdir -v $HOME/.dotfiles_old
	elif [ $kernel == 'Linux' ]; then
		echo "\nMaking backup directory..."
		mkdir -v $HOME/.dotfiles_old
	fi
fi

for file in $HOME/.vim $HOME/.vimrc $HOME/.gvimrc $HOME/.tmux.conf $HOME/.zsh $HOME/.zshrc $HOME/.bash $HOME/.bashrc $HOME/.profile $HOME/.gitconfig $HOME/.gitmessage.txt
do
	if [ -e $file ] && [ ! -L $file ]; then
		if [ $kernel == 'Darwin' ]; then
			mv -v $file $HOME/.dotfiles_old
		elif [ $kernel == 'Linux' ]; then
			mv -v $file $HOME/.dotfiles_old
		fi
	fi
done

if [ $kernel == 'Darwin' ]; then
	# Making symlinks to Vim files, add yours as you need
	echo "Making symlinks to Vim files"
	ln -sfn -v $dotfiles_dir/vim $HOME/.vim
	ln -sfn -v $dotfiles_dir/vim/vimrc.vim $HOME/.vimrc
	ln -sfn -v $dotfiels_dir/vim/gvimrc.vim $HOME/.gvimrc
	echo "Done at [$time]...\n"

	# Making symlinks to Tmux files, add yours as you need
	echo "Making symlinks to Tmux files"
	ln -sfn -v $dotfiles_dir/tmux/tmux.conf $HOME/.tmux.conf
	echo "Done at  [$time]...\n"

	# Making symlinks to shell files, add yours as you need
	echo 'Making symlinks shell files'
	ln -sfn -v $dotfiles_dir/shells/zsh $HOME/.zsh
	ln -sfn -v $dotfiles_dir/shells/zsh/zshrc $HOME/.zshrc
	ln -sfn -v $dotfiles_dir/shells/bash $HOME/.bash
	ln -sfn -v $dotfiles_dir/shells/bash/bashrc
	ln -sfn -v $dotfiles_dir/shells/profile $HOME/.profile
	echo "Done at [$time]...\n"

	# Making symlinks to Git files, add yours as you need
	echo 'Making symlinks to Git files'
	ln -sfn -v $dotfiles_dir/git/gitconfig $HOME/.gitconfig
	ln -sfn -v $dotfiles_dir/git/gitmessage.txt $HOME/.gitmessage.txt
	echo "Done at [$time]...\n"
elif [ $kernel == 'Linux' ]; then
	# Making symlinks to Vim files, add yours as you need
	echo 'Making symlinks to Vim files'
	ln -sfn -v $dotfiles_dir/vim $HOME/.vim
	ln -sfn -v $dotfiles_dir/vim/vimrc $HOME/.vimrc
	ln -sfn -v $dotfiles_dir/vim/gvimrc $HOME/.gvimrc
	echo "Done at [$time]...\n"

	# Making symlinks to Tmux files, add yours as you need
	echo "Making symlinks to Tmux files"
	ln -sfn -v $dotfiles_dir/vim/tmux.conf $HOME/.tmux.conf
	echo "Done at  [$time]...\n"

	# Making symlinks to shell files, add yours as you need
	echo 'Making symlinks shell files'
	ln -sfn -v $dotfiles_dir/shells/zsh $HOME/.zsh
	ln -sfn -v $dotfiles_dir/shells/zsh/zshrc $HOME/.zshrc
	ln -sfn -v $dotfiles_dir/shells/bash $HOME/.bash
	ln -sfn -v $dotfiles_dir/shells/bash/bashrc $HOME/.bashrc
	ln -sfn -v $dotfiles_dir/shells/profile $HOME/.profile
	echo "Done at [$time]...\n"

	# Making symlinks to Git files, add yours as you need
	echo 'Making symlinks to Git files'
	ln -sfn -v $dotfiles_dir/git/gitconfig $HOME/.gitconfig
	ln -sfn -v $dotfiles_dir/git/gitmessage.txt $HOME/.gitmessage.txt
	echo "Done at [$time]...\n"
fi

echo "Finished making new symlinks, at [$time]..."
