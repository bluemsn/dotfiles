#!/bin/zsh
# =============================================================================
# brew_installs.sh
# -----------------------------------------------------------------------------
# This file will automatically install some important stuff using Homebrew.
# =============================================================================
# Eduan Lavaque <eduan@snapsimpletech.com>
# Licensed under the MIT license (htpp://eduan.mit-license.org/2012-2013)
# =============================================================================

# Check if Homebrew is installed
if test ! $(which brew); then
	echo 'You need to install Homebrew!'
	echo 'I will install it for you now...'
	ruby -e "$(curl -fsSkL raw.github.com/mxcl/homebrew/go)"
	brew doctor
	echo 'Follow the instructions "brew doctor" gives you. Then run this file again.'
else
	# Install little utilities
	brew install ack
	brew install par
	brew install hub
	brew install figlet

	# Install tmux
	brew install tmux
	brew install reattach-to-user-namespace

	# Update shells
	brew install bash
	brew install zsh

	# Install Mercurial
	brew install hg

	# Install/update Vim
	brew install macvim --override-system-vim
	#ln -s /usr/local/Cellar/macvim/7.3-65/MacVim.app /Applications

	echo 'Done, I installed your stuff, you can thank me later.'
fi
