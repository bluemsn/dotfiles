#!/bin/zsh
# =============================================================================
# brew_installs.sh
# -----------------------------------------------------------------------------
# This file will automatically setup a SSH key for you.
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

if ! $(which ssh-keygen); then
	sudo pacman -S openssh
fi

# Generate a SSH key
ssh-keygen -t rsa -C "eduan@snapsimpletech.com"

# Make sure to give it the correct directory...
# Enter the passphrase twice...

# Re-do it in a better encryption method?
#openssl pkcs8 -topk8 -v2 des3 -in ~/.ssh/id_rsa -out ~/.ssh/id_rsa

if [ `uname -s` == 'Darwin' ]; then
	# Copy the contents of 'id_rsa.pub' (SSH key) to my clipboard
	pbcopy < ~/.ssh/id_rsa.pub
elif [ `uname -s` == 'Linux' ]; then
	if ! $(which xclip); then
		# Install `xclip`, assuming you're on Arch Linux
		sudo pacman -S xclip
	fi

	# Copy the contents of 'id_rsa.pub' (SSH key) to my clipboard
	xclip -sel clip < ~/.ssh/github.pub
fi
