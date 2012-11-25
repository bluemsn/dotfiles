#!/bin/zsh
#####################
# delete_symlinks.sh
# This file will delete all your symlinks defined here
#####################
# Copyright (C) 2012 Eduan Lavaque <eduan@snapsimpletech.com>
# Licenced under the MIT license (http://mit-license.org/)
#####################

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

echo "Started deleting old symlinks, at [$time]...\n"

if [ $kernel == 'Darwin' ]; then
	# Remove Vim symlinks
	echo 'Deleting Vim symlinks...'
	rm -v ~/.vim
	rm -v ~/.vimrc
	rm -v ~/.gvimrc
	echo "Done at [$time]...\n"

	# Remove Zsh symlinks
	echo 'Deleting Zsh symlinks...'
	rm -v ~/.zsh
	rm -v ~/.zshrc
	echo "Done at [$time]...\n"

	# Remove Git symlinks
	echo 'Deleting Git symlinks...'
	rm -v ~/.gitconfig
	rm -v ~/.gitmessage.txt
	echo "Done at [$time]...\n"
fi

if [ $kernel == 'Linux' ]; then
	# Remove Vim symlinks
	rm -v /home/$user/.vim
	rm -v /home/$user/.vimrc
	rm -v /home/$user/.gvimrc

	# Remove Zsh symlinks
	rm -v /home/$user/.zsh
	rm -v /home/$user/.zshrc

	# Remove Git symlinks
	rm -v /home/$user/.gitconfig
	rm -v /home/$user/.gitmessage.txt
fi

echo "Finished deleting old symlinks, at [$time]..."
