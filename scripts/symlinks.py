git#!/usr/bin/python2
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
date = strftime('%Y-%m-%d', gmtime())

# Backup folder
backup_folder = backup_main_folder + '/' + date

# Generate the backup folder if it doesn't already exist
if not os.path.exists(backup_folder):
	os.mkdir(backup_folder, 0755)

# Tuple with all the locations of the files from $DOTROOT (in a manner of speech)
source = ['git/gitmessate.txt', 'shells/bash', 'shells/bash/bash_profile', 'shells/bash/bashrc', 'shells/zsh', 'shells/zsh/zshrc', 'shells/profile', 'tmux/tmux.conf', 'vim', 'vim/gvimrc.vim', 'vim/vimrc.vim']
# Tuple with all the destinations of the files in the dotfiles
destination = ['.gitmessage.txt', '.bash', '.bash_profile', '.bashrc', '.zsh', '.zshrc', '.profile', '.tmux.conf', '.vim', '.gvimrc', '.vimrc']

for src in source:
	for dest in destination:
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
		final_src = expanduser('~') + '/dotfiles/' + src
		# The actual destination that's going to be used
		final_dest = expanduser('~') + dest

		# Make symlink
		os.symlink(final_src, final_dest)

		# Print message regarding currently symlink...
		print '%s -> %s' % (final_dest, final_src)

print 'Finished making symlinks!'
