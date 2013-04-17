#!/usr/bin/python2
# symlinks.py by Eduan Lavaque <eduan@snapsimpletech.com>
# Licensed under the MIT license (http://eduan.mit-license.org/)
# This file will generate my symlinks. For new installs.
# -*- coding: utf-8 -*-
import os
from os.path import expanduser, exists, islink
from time import gmtime, strftime

# Figure out the home folder
home_folder = expanduser('~')

# Backup main folder, user's home folder plus directory
backup_main_folder = home_folder + '/.dotfiles_old'

# If the main backup folder doesn't exist then create it with 0755 permissions
if not os.path.exists(backup_main_folder):
	os.mkdir(backup_main_folder)

# Get the current time in a "YYYY-MM-DD" format
date = strftime('%Y-%m-%d %H:%M', gmtime())

# Backup folder
backup_folder = backup_main_folder + '/' + date

# Generate the backup folder if it doesn't already exist
if not os.path.exists(backup_folder):
	os.mkdir(backup_folder)

# Ask dotfiles folder from user, from $HOME
dotroot_input = raw_input("Where is your dotfiles folder located?\n~/")
# Strip possible trailing slash to avoid problems...
if dotroot_input.endswith('/'):
	dotroot_input = dotroot_input[:-1]

# Set the dotfiles root folder
dotroot = home_folder + '/' + dotroot_input

# On the left side, the source (from $DOTROOT). On the right side, the destination (from $HOME)
files = {
	'/linux/xinitrc': '.xinitrc',
	'/random/gitmessage.txt': '.gitmessage.txt',
	'/shells/bash': '.bash',
	'/shells/bash/bash_profile': '.bash_profile',
	'/shells/bash/bashrc': '.bashrc',
	'/shells/zsh': '.zsh',
	'/shells/zsh/zshrc': '.zshrc',
	'/shells/profile': '.profile',
	'/ssh': '.ssh',
	'/tmux/tmux.conf': '.tmux.conf',
	'/vim': '.vim',
	'/vim/gvimrc.vim': '.gvimrc',
	'/vim/vimrc.vim': '.vimrc',
}

backup_boolean = raw_input('Do you want to backup your old files? [Y/n] ').lower()

if backup_boolean == 'y' or 'ye' or 'yes' or '':
	print 'Gonna do backups of actual files and delete symlinks. Backed up files will be moved to "%s"' % (backup_folder)
elif backup_boolean == 'n' or 'no':
	print 'OK, not gonna do backup of the files... Don\'t blame me if you lose anything. :)'

for src, dest in files.iteritems():
	if backup_boolean == 'y' or 'ye' or 'yes':
		# If the file exists but it's a symlink...
		if os.path.exists(dest) and os.path.islink(dest):
			# Then delete it
			os.remove(dest)
		# If the file exists and it's NOT a symlink...
		elif os.path.exists(dest) and not os.path.islink(dest):
			# Figure out the backup destination
			backup_dest = backup_folder + '/' + dest
			# Move the file as a backup to the backup destination
			os.rename(dest, backup_dest)

	# The actual source that's going to be used
	final_src = dotroot + src
	# The actual destination that's going to be used
	final_dest = home_folder + '/' + dest

	# Make symlink
	os.symlink(final_src, final_dest)

	# Print message regarding currently symlink...
	print '%s -> %s' % (final_dest, final_src)

print 'Finished making symlinks!'
