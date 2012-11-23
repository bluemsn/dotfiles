#####################
# setup_vim.sh
# This file does a couple of things that you would otherwise need to do
# manually in order to setup Vim from zero
#####################
# Copyright (C) 2012 Eduan Lavaque <eduan@snapsimpletech.com>
# Licenced under the MIT license (http://mit-license.org/)
#####################

# Fix a Python error that you get when using 'vim' in the Terminal
sudo mkdir -p /usr/include/python2.7
sudo ln -s /System/Library/Frameworks/Python.framework/Versions/Current/include/python2.7/pyconfig.h /usr/include/python2.7

# Install Vundle and all it's bundles
git clone git://github.com/gmarik/vundle.git ./bundle/vundle
vim -c ':BundleInstall!' \
	-c ':qa!'
