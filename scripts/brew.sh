#!/bin/sh
# Check if Homebrew is installed
if [ test ! $(which brew) ]; then
	# Install Homebrew if it isn't already installed
	ruby -e "$(curl -fsSkL raw.github.com/mxcl/homebrew/go)"
fi

brew install par
brew install bash
brew install zsh
brew install urxvt
brew install hg
brew install git
brew install python
brew install python3
brew install markdown
brew install leiningen
brew install node
brew install tmux
brew install reattach-to-user-namespace --wrap-pbcopy-and-pbpaste --wrap-launchctl
brew install macvim --env-std --custom-icons --override-system-vim --with-python3
brew install emacs --cocoa --srgb

mkdir -pv ~/Applications
brew linkapps
