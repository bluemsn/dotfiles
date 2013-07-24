#!/usr/bin/zsh
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
	# Install Homebrew if it isn't already installed
	ruby -e "$(curl -fsSkL raw.github.com/mxcl/homebrew/go)"
fi

# Install little utilities
#brew install ack
#brew install the_silver_searcher
brew install par
#brew install hub
brew install figlet
# Not really brew but to hell with it
git clone https://github.com/drbunsen/formd.git ~/bin

# Update shells
brew install bash
brew install zsh

# Install Mercurial
brew install hg

# Install/update Python
brew install python
brew install python3
brew install markdown
brew install clojure
brew install leiningen

# Install Node.js and related stuff
brew install node
curl https://npmjs.org/install.sh | sh
sudo npm install -g docpad@6
sudo npm install -g grunt-cli

# Install tmux
brew install tmux
brew install reattach-to-user-namespace
#brew install https://github.com/downloads/zolrath/wemux/wemux.rb

# Install/update Vim
brew install macvim --env-std --custom-icons --override-system-vim --with-python3

# Link all the installed apps
mkdir -pv ~/Applications
brew linkapps
