#!/usr/bin/zsh
# git.sh by Eduan Lavaque <eduanlavaque@gmail.com>
# Licensed under the MIT license (http://eduan.mit-license.org/)

# Setup Git username
git config --global user.name "Greduan"

# Setup Git email
git config --global user.email "eduanlavaque+git@gmail.com"

# Setup commit editor
git config --global core.editor vim

# Setup commit template
#git config --global commit.template $HOME/.gitmessage.txt

# Setup script to use to show commit history etc.
git config --global core.pager 'less'

# Enable auto-correct
git config --global help.autocorrect 1

# Use colors
git config --global color.ui true

# Automatically use LF line endings when commiting
git config --global core.eol lf
git config --global core.autocrlf input

# Create Git aliases...
git config --global alias.logg 'log --graph --decorate --oneline --abbrev-commit --all'

# For Git 2.x, set default method of pushing
git config --global push.default simple
