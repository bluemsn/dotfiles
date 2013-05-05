#!/usr/bin/python2
# symlinks.py by Eduan Lavaque <eduan@snapsimpletech.com>
# Licensed under the MIT license (http://eduan.mit-license.org/)
# This file will generate my symlinks. For new installs.
# -*- coding: utf-8 -*-
import os, sh
from os.path import expanduser, exists
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

commands = os.listdir(dotroot + '/bin/')

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

for cmd in commands:
	# The location of the command script
	final_src = dotroot + '/bin/' + cmd
	# The destination of the command script symlink
	final_dest = '/usr/local/bin/' + cmd

	# Ask the user for sudo access
	with sh.sudo:
		# If the file exists...
		if os.path.exists(final_dest):
			# Then delete it
			sh.rm('-r', final_dest)

		# Make symlinks to commands
		sh.ln('-s', final_src, final_dest)

	# Print message regarding symlink to current command...
	print 'Finished installing `%s` script under "/usr/local/bin/"' % (cmd)

print 'Finished making symlinks!'
