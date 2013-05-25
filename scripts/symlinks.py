#!/usr/bin/python2
# symlinks.py by Eduan Lavaque <eduan@snapsimpletech.com>
# Licensed under the MIT license (http://eduan.mit-license.org/)
# This file will generate my symlinks. For new installs.
# -*- coding: utf-8 -*-
import os
from os.path import expanduser, exists

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
	'/linux/xinitrc'            : '.xinitrc',
	'/random/gitmessage.txt'    : '.gitmessage.txt',
	'/shells/bash'              : '.bash',
	'/shells/bash/bash_profile' : '.bash_profile',
	'/shells/bash/bashrc'       : '.bashrc',
	'/shells/zsh'               : '.zsh',
	'/shells/zsh/zshrc'         : '.zshrc',
	'/shells/profile'           : '.profile',
	'/ssh'                      : '.ssh',
	'/tmux/tmux.conf'           : '.tmux.conf',
	'/vim'                      : '.vim',
	'/vim/gvimrc.vim'           : '.gvimrc',
	'/vim/vimrc.vim'            : '.vimrc',
}

# Set the commands that will be symlinked...
commands = {
	'/bin/tmux-vim-select-pane' : 'tmux-vim-select-pane',
}

for src, dest in files.iteritems():
	# The actual source that's going to be used
	final_src = dotroot + src
	# The actual destination that's going to be used
	final_dest = home_folder + '/' + dest

	# If the file exists...
	if os.path.exists(final_dest):
		# Then delete it
		os.remove(final_dest)

	# Make symlink
	os.symlink(final_src, final_dest)

	# Print message regarding current symlink...
	print '%s -> %s' % (final_dest, final_src)

for src, cmd in commands.iteritems():
	# The location of the command script
	final_src = dotroot + src
	# The destination of the command script symlink
	final_dest = '/usr/local/bin/' + cmd

	# If the file exists...
	if os.path.exists(final_dest):
		# Then delete it
		os.popen('sudo rm ' + final_dest, 'w')

	# Make symlinks to commands
	os.popen('sudo ln -vs ' + final_src + ' ' + final_dest, 'w')

print 'Finished making symlinks!'
