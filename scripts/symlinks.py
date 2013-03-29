#!/usr/bin/python2
# symlinks.py by Eduan Lavaque <eduan@snapsimpletech.com>
# Licensed under the MIT license (http://eduan.mit-license.org/)
# This file will generate my symlinks. For new installs.
# -*- coding: utf-8 -*-
import os
from os.path import expanduser
from time import gmtime, strftime

# Backup main folder, user's home folder plus directory
backup_main_folder = expanduser('~') + '/.dotfiles_old'

# If the main backup folder doesn't exist then create it with 0755 permissions
if not os.path.exists(backup_main_folder):
	os.mkdir(backup_main_folder, 0755)

# Get the current time in a "YYYY-MM-DD" format
date = strftime('%Y-%m-%d %H:%M', gmtime())

# Backup folder
backup_folder = backup_main_folder + '/' + date

# Generate the backup folder if it doesn't already exist
if not os.path.exists(backup_folder):
	os.mkdir(backup_folder, 0755)

# Set a definite $DOTROOT sorta variable
dotroot = expanduser('~') + '/dotfiles'

# On the left side, the source (from $DOTROOT). On the right side, the destination (from $HOME)
files = {
	'/git/gitmessage.txt': '.gitmessage.txt',
	'/shells/bash': '.bash',
	'/shells/bash/bash_profile': '.bash_profile',
	'/shells/bash/bashrc': '.bashrc',
	'/shells/zsh': '.zsh',
	'/shells/zsh/zshrc': '.zshrc',
	'/shells/profile': '.profile',
	'/tmux/tmux.conf': '.tmux.conf',
	'/vim': '.vim',
	'/vim/gvimrc.vim': '.gvimrc',
	'/vim/vimrc.vim': '.vimrc',
}

for src, dest in files.iteritems():
	# If the file exists but it's a symlink...
	if os.path.exists(dest) and os.path.islink(dest):
		# Then delete it
		os.remove(dest)
	# If the file exists and it's NOT a symlink...
	elif os.path.exists(dest) and not os.path.islink(dest):
		# Figure out the backup destination
		backup_dest = backup_folder + dest
		# Move the file as a backup to the backup destination
		os.rename(dest, backup_dest)

	# The actual source that's going to be used
	final_src = dotroot + src
	# The actual destination that's going to be used
	final_dest = expanduser('~') + dest

	# Make symlink
	os.symlink(final_src, final_dest)

	# Print message regarding currently symlink...
	print '%s -> %s' % (final_dest, final_src)

print 'Finished making symlinks!'
