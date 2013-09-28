#!/usr/bin/zsh
# brew.sh by Eduan Lavaque <eduanlavaque@gmail.com>
# Licensed under the MIT license (http://eduan.mit-license.org/)

# Check if Homebrew is installed
if test ! $(which brew); then
	# Install Homebrew if it isn't already installed
	ruby -e "$(curl -fsSkL raw.github.com/mxcl/homebrew/go)"
fi

# Utilities
brew install par
#brew install figlet
#brew install ack
#brew install the_silver_searcher

# CLI shells
brew install bash
brew install zsh
brew install urxvt

# VCS software
brew install hg
brew install git

# Languages
brew install python
brew install python3
brew install markdown
brew install leiningen
brew install clojurescript
brew install clisp
brew install clozure-cl

# Node.js stuff
brew install node
sudo npm install -g docpad@6
sudo npm install -g grunt-cli
sudo npm install -g coffee-script

# Tmux
brew install tmux
brew install reattach-to-user-namespace --wrap-pbcopy-and-pbpaste --wrap-launchctl

# Install/update Vim
brew install macvim --env-std --custom-icons --override-system-vim --with-python3
brew install vim --override-system-vi

# Link all the installed apps
mkdir -pv ~/Applications
brew linkapps
