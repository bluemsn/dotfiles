#!/bin/zsh
# =============================================================================
# delete_symlinks.sh
# -----------------------------------------------------------------------------
# This file will delete all your symlinks defined here, it will do it in a nice
# and clean manner, telling you what's happening the whole way.
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

# Recognize the dotfiles directory
dotfiles_dir="$HOME/dotfiles"

# Exit the script if I'm not sure of what Unix kernel this is being run on
if [ $kernel == 'Darwin' ] || [ $kernel == 'Linux' ]; then
	echo "Started making new symlinks, at [$time]...\n"
else
	echo "I do not recognize this Unix kernel."
	exit
fi

mkdir -v $HOME/.dotfiles_old
for file in $HOME/.vim $HOME/.vimrc $HOME/.gvimrc $HOME/.tmux.conf $HOME/.zsh $HOME/.zshrc $HOME/.bash $HOME/.bashrc $HOME/.profile $HOME/.gitconfig $HOME/.gitmessage.txt
do
	if [ -e $file ] && [ ! -L $file ]; then
		if [ $kernel == 'Darwin' ]; then
			cp -v $file $HOME/.dotfiles_old
		elif [ $kernel == 'Linux' ]; then
			cp -v $file $HOME/.dotfiles_old
		fi
	fi
done

# Remove Vim symlinks
echo 'Deleting Vim symlinks...'
rm -v $HOME/.vim
rm -v $HOME/.vimrc
rm -v $HOME/.gvimrc
echo "Done at [$time]...\n"

# Remove Tmux symlinks
echo "Deleting Tmux symlinks..."
rm -v $HOME/.tmux.conf
echo "Done at  [$time]...\n"

# Shell symlinks
echo 'Deleting shell related symlinks...'
rm -v $HOME/.zsh
rm -v $HOME/.zshrc
rm -v $HOME/.bash
rm -v $HOME/.bashrc
rm -v $HOME/.profile
echo "Done at [$time]...\n"

# Remove Git symlinks
echo 'Deleting Git symlinks...'
rm -v $HOME/.gitconfig
rm -v $HOME/.gitmessage.txt
echo "Done at [$time]...\n"

# Remove random symlinks
echo 'Deleting symlinks to KeyRemap4MacBook stuff...'
rm -v $HOME/Library/Application\ Support/KeyRemap4MacBook/private.xml
echo "Done at [$time]...\n"

echo "Finished deleting old symlinks, at [$time]..."
