#!/bin/zsh
# =============================================================================
# setup_vim.sh
# -----------------------------------------------------------------------------
# This script will do a couple of things for you that you would normally do
# when installing MacVim in a new Mac
# =============================================================================
# Eduan Lavaque <eduan@snapsimpletech.com>
# Licensed under the MIT license (htpp://eduan.mit-license.org/)
# =============================================================================

# Fix a Python error that you get when using 'vim' in the Terminal
sudo mkdir -p /usr/include/python2.7
sudo ln -s /System/Library/Frameworks/Python.framework/Versions/Current/include/python2.7/pyconfig.h /usr/include/python2.7

# Install Vundle and all it's bundles
git clone git://github.com/gmarik/vundle.git ./../vim/bundle/vundle
vim +BundleInstall +BundleClean +qa
