#!/bin/zsh
# =============================================================================
# symlinks.sh
# -----------------------------------------------------------------------------
# Automatically makes a symlink to all the files that have a .symlink extension
# in their filename.
# =============================================================================
# Eduan Lavaque <eduan@snapsimpletech.com>
# Licensed under the MIT license (http://eduan.mit-license.org)
# =============================================================================

# Just ask the user where the dotfiles are, simple...
read -p "What's your dotfiles folder? Full path please...\n" dotfiles_root

# For each file in the dotfiles folder with `.symlink` as it's last file name
# extension...
for source in `find $dotfiles_root -maxdepth 2 -name \*.symlink`
do
	# Set the destination as `$HOME/.file` instead of `$HOME/.file.txt.symlink`
	# for example.
	dest = "$HOME/.`basename \"${source%%.*}\"`"

	# Make a symlink to each of these files, delete previous files if they
	# exist(ed).
	ln -sfn -v $source $dest
done