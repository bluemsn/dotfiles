#!/bin/zsh
# =============================================================================
# brew_installs.sh
# -----------------------------------------------------------------------------
# This file will automatically install some important stuff using Homebrew.
# =============================================================================
# Eduan Lavaque <eduan@snapsimpletech.com>
# Licensed under the MIT license (htpp://eduan.mit-license.org/)
# =============================================================================

# Check if Homebrew is installed
if test ! $(which brew); then
	echo 'You need to install Homebrew!'
	echo 'I will install it for you now...'
	ruby -e "$(curl -fsSkL raw.github.com/mxcl/homebrew/go)"
	echo 'Follow the instructions 'brew doctor' gives you. Then run this file again.'
	brew doctor
else
	brew install ack
	brew install par
	brew install hub
	brew install https://raw.github.com/Homebrew/homebrew-dupes/master/grep.rb
	brew install reattach-to-user-namespace
	brew install bash
	echo 'Done, I installed your stuff, you can thank me later.'
fi
