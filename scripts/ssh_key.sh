#!/bin/zsh
# =============================================================================
# brew_installs.sh
# -----------------------------------------------------------------------------
# This file will automatically setup a SSH key for you with GitHub. Creating it
# in your OS and making GitHub accept it as well.
# =============================================================================
# Eduan Lavaque <eduan@snapsimpletech.com>
# Licensed under the MIT license (http://eduan.mit-license.org/)
# =============================================================================

# Make sure the .ssh directory exists
if [ ! -d ~/.ssh ]; then
    # Create it if it doesn't exists
    mkdir -v ~/.ssh
elif [ -d ~/.ssh ]; then
    # If it does exist then make a backup of all the files in it
    mkdir -v ~/.ssh/key_backup
    cp id_rsa* key_backup
    rm id_rsa*
fi

# Generate a SSH key
ssh-keygen -t rsa -C "eduan@snapsimpletech.com"

# Make sure to give it the correct directory...
# Enter the passphrase twice...

if [ `uname -s` == 'Darwin' ]; then
	# Copy the contents of 'id_rsa.pub' (SSH key) to my clipboard
	pbcopy < ~/.ssh/id_rsa.pub
elif [ `uname -s` == 'Linux' ]; then
	# Install `xclip`, assuming you're on Arch Linux
	pacman -S xclip

	# Copy the contents of 'id_rsa.pub' (SSH key) to my clipboard
	xclip -sel clip < ~/.ssh/id_rsa.pub
fi