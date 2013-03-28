#!/bin/zsh
# =============================================================================
# setup_git.sh
# -----------------------------------------------------------------------------
# Sets up Git for first use.
# =============================================================================
# Eduan Lavaque <eduan@snapsimpletech.com>
# Licensed under the MIT license (http://eduan.mit-license.org)
# =============================================================================

# Setup Git username
git config --global user.name "Eduan Lavaque"

# Setup Git email
git config --global user.email "eduan@snapsimpletech.com"

# Setup commit editor
git config --global core.editor vim

# Setup commit template
git config --global commit.template $HOME/.gitmessage.txt

# Setup script to use to show commit history etc.
git config --global core.pager 'less'

# Enable auto-correct
git config --global help.autocorrect 1

# Use colors
git config --global color.ui true

# Automatically use LF line endings when commiting
git config --global core.autocrlf input

# Create Git aliases...
git config --global alias.logg 'log --graph --decorate --oneline --abbrev-commit --all'