#!/usr/bin/zsh
# pathogen.sh by Eduan Lavaque <eduanlavaque@gmail.com>
# Licensed under the MIT license (http://eduan.mit-license.org/)

install() {
	git clone git://github.com/$1.git
}

uninstall() {
	rm -rf ~/.vim/bundle/$1
}

update() {
	git pull origin master $1
}
