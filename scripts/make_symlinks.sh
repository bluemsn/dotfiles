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

if [ $kernel == 'Darwin' ]; then
	$HOME = /Users/$user
elif [ $kernel == 'Linux' ]; then
	$HOME = /home/$user
fi

# This if statement is to know the correct location of the script, if the
# shell's location is the same as the script's location
if [ $script_dir == '.' ]; then
	script_dir="$current_dir"
fi

# Let the script know what the dotfiles dir is
dotfiles_dir="$HOME/dotfiles"

# Exit the script if I'm not sure of what Unix kernel this is being run on
if [ $kernel == 'Darwin' ] || [ $kernel == 'Linux' ]; then
	echo "Started making new symlinks, at [$time]...\n"
else
	echo "I do not recognize this Unix kernel."
	exit
fi

echo "First I'm gonna backup any of the files that I find, just in case..."
for file in $HOME/.vim $HOME/.vimrc $HOME/.gvimrc $HOME/.tmux.conf $HOME/.zsh $HOME/.zshrc $HOME/.bash $HOME/.bashrc $HOME/.profile $HOME/.gitconfig $HOME/.gitmessage.txt
do
	if [ -e $file ] && [ ! -L $file ]; then
		mkdir -v $HOME/.dotfiles_old
		mv -f -v $file $HOME/.dotfiles_old
	fi
done
echo ''

# Making symlinks to Vim files, add yours as you need
echo 'Making symlinks to Vim files...'
ln -sfn -v $dotfiles_dir/vim $HOME/.vim
ln -sfn -v $dotfiles_dir/vim/vimrc.vim $HOME/.vimrc
ln -sfn -v $dotfiles_dir/vim/gvimrc.vim $HOME/.gvimrc
echo "Done at [$time]...\n"

# Making symlinks to Tmux files, add yours as you need
echo 'Making symlinks to tmux files...'
ln -sfn -v $dotfiles_dir/tmux/tmux.conf $HOME/.tmux.conf
echo "Done at  [$time]...\n"

# Making symlinks to shell files, add yours as you need
echo 'Making symlinks to shell files...'
ln -sfn -v $dotfiles_dir/shells/zsh $HOME/.zsh
ln -sfn -v $dotfiles_dir/shells/zsh/zshrc $HOME/.zshrc
ln -sfn -v $dotfiles_dir/shells/bash $HOME/.bash
ln -sfn -v $dotfiles_dir/shells/bash/bash_profile $HOME/.bash_profile
ln -sfn -v $dotfiles_dir/shells/bash/bashrc $HOME/.bashrc
ln -sfn -v $dotfiles_dir/shells/profile $HOME/.profile
echo "Done at [$time]...\n"

# Making symlinks to Git files, add yours as you need
echo 'Making symlinks to Git files...'
ln -sfn -v $dotfiles_dir/git/gitconfig $HOME/.gitconfig
ln -sfn -v $dotfiles_dir/git/gitmessage.txt $HOME/.gitmessage.txt
echo "Done at [$time]...\n"

# Making symlinks to random files, add yours as needed
echo 'Making symlinks to KeyRemap4MacBook stuff...'
ln -sfn -v $dotfiles_dir/random/keyremap4macbook_private.xml $HOME/Library/Application\ Support/KeyRemap4MacBook/private.xml
echo "Done at [$time]...\n"

echo "Finished making new symlinks, at [$time]..."
