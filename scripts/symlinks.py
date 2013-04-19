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

# Ask dotfiles folder from user, from $HOME
dotroot_input = raw_input("Where is your dotfiles folder located?\n~/")
# Strip possible trailing slash to avoid problems...
if dotroot_input.endswith('/'):
	dotroot_input = dotroot_input[:-1]

# Set the dotfiles root folder
dotroot = home_folder + '/' + dotroot_input

# On the left side, the source (from $DOTROOT). On the right side, the destination (from $HOME)
files = {
	'/linux/openbox': '.config/openbox'
	'/linux/tint2': '.config/tint2/tint2rc'
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

for src, dest in files.iteritems():
	# If the file exists...
	if os.path.exists(dest)
		# Then delete it
		os.remove(dest)

	# The actual source that's going to be used
	final_src = dotroot + src
	# The actual destination that's going to be used
	final_dest = home_folder + '/' + dest

	# Make symlink
	os.symlink(final_src, final_dest)

	# Print message regarding currently symlink...
	print '%s -> %s' % (final_dest, final_src)

print 'Finished making symlinks!'
