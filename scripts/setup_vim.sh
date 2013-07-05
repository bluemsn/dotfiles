#!/usr/bin/zsh
# =============================================================================
# setup_vim.sh
# -----------------------------------------------------------------------------
# This script will do a couple of things for you that you would normally do
# when installing MacVim in a new Mac
# =============================================================================
# Eduan Lavaque <eduan@snapsimpletech.com>
# Licensed under the MIT license (htpp://eduan.mit-license.org/2012-2013)
# =============================================================================

# Fix a Python error that you get when using 'vim' in the Terminal in Mac
if [ `uname -s` == 'Darwin' ]; then
	echo "Currently fixing possible Python errors..."
	sudo mkdir -p /usr/include/python2.7
	sudo ln -s /System/Library/Frameworks/Python.framework/Versions/Current/include/python2.7/pyconfig.h /usr/include/python2.7
fi

# Install Vundle
echo "Installing Vundle..."
git clone git://github.com/gmarik/vundle.git ~/.vim/bundle/vundle
