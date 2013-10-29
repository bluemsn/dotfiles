#!/usr/bin/zsh
# brew.sh by Eduan Lavaque <eduanlavaque@gmail.com>
# Licensed under the MIT license (http://eduan.mit-license.org/)

# Check if Homebrew is installed
if test ! $(which brew); then
	# Install Homebrew if it isn't already installed
	ruby -e "$(curl -fsSkL raw.github.com/mxcl/homebrew/go)"
fi

brew install par
#brew install figlet
#brew install ack
#brew install the_silver_searcher
brew install bash
brew install zsh
brew install urxvt
brew install hg
brew install git
brew install python
brew install python3
brew install markdown
brew install leiningen
brew install clojurescript
brew install clisp
brew install clozure-cl
brew install node
sudo npm install -g docpad@6
sudo npm install -g grunt-cli
sudo npm install -g coffee-script
brew install tmux
brew install reattach-to-user-namespace --wrap-pbcopy-and-pbpaste --wrap-launchctl
brew install macvim --env-std --custom-icons --override-system-vim --with-python3
brew install vim --override-system-vi
brew install emacs
mkdir -pv ~/Applications
brew linkapps
brew unlink macvim && brew link vim
