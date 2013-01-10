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
	ruby -e "$(curl -fsSkL raw.github.com/mxcl/homebrew/go)"
fi

# Install little utilities
brew install ack
brew install par
brew install hub
brew install figlet

# Install Markdown and MultiMarkdown
brew install markdown
brew install multimarkdown

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

# Install/update Vim
brew install macvim --custom-icons --override-system-vim --with-python3

# Link all the installed apps
mkdir ~/Applications
brew linkapps

echo 'Done, I installed your stuff, you can thank me later.'
