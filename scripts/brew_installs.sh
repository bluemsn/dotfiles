#!/bin/zsh
# =============================================================================
# brew_installs.sh
# -----------------------------------------------------------------------------
# This file will automatically install some important stuff using Homebrew.
# =============================================================================
# Eduan Lavaque <eduan@snapsimpletech.com>
# Licensed under the MIT license (http://eduan.mit-license.org/2012-2013)
# =============================================================================

# Check if Homebrew is installed
if test ! $(which brew); then
	echo 'You need to install Homebrew!'
	echo 'I will install it for you now...'
	ruby -e "$(curl -fsSkL raw.github.com/mxcl/homebrew/go)"
else
	# Install little utilities
	brew install ack
	brew install par
	brew install hub
	brew install figlet

	# Update shells
	brew install bash
	brew install zsh

	# Install Mercurial
	brew install hg

	# Install/update Python
	brew install python 
	brew install python3 

	# Install tmux
	brew install tmux
	brew install reattach-to-user-namespace

	# Install Markdown and MultiMarkdown
	brew install markdown 
	brew install multimarkdown 

	# Install/update Vim
	brew install macvim --custom-icons --override-system-vim --with-python3 

	echo 'Done, I installed your stuff, you can thank me later.'
fi
