#!/usr/bin/zsh
# vim.sh by Eduan Lavaque <eduanlavaque@gmail.com>
# Licensed under the MIT license (http://eduan.mit-license.org/)

# Make folders for Pathogen
mkdir -p ~/.vim/autoload ~/.vim/bundle

# Install Pathogen
curl -Sso ~/.vim/autoload/pathogen.vim https://raw.github.com/tpope/vim-pathogen/master/autoload/pathogen.vim

# Install all the plugins:
# General plugins...
git clone https://github.com/chreekat/vim-paren-crosshairs.git
git clone https://github.com/drmikehenry/vim-fixkey.git
git clone https://github.com/kana/vim-smartinput.git
git clone https://github.com/kien/rainbow_parentheses.vim.git
git clone https://github.com/rhysd/clever-f.vim.git
git clone https://github.com/scrooloose/nerdtree.git
git clone https://github.com/ConradIrwin/vim-bracketed-paste.git
git clone https://github.com/sjl/vitality.vim.git
git clone https://github.com/svermeulen/vim-easyclip.git
git clone https://github.com/tomtom/tcomment_vim.git
git clone https://github.com/tpope/vim-repeat.git
git clone https://github.com/tpope/vim-surround.git
git clone https://github.com/troydm/easybuffer.vim.git
#git clone https://github.com/tpope/vim-fugitive.git
git clone https://github.com/gorodinskiy/vim-coloresque.git
git clone https://github.com/mattn/emmet-vim.git
# Color schemes...
git clone https://github.com/Greduan/vim-colors-solarized.git
# Syntaxes...
git clone https://github.com/AndrewRadev/vim-eco.git
git clone https://github.com/cakebaker/scss-syntax.vim.git
git clone https://github.com/kchmck/vim-coffee-script.git
git clone https://github.com/mutewinter/vim-css3-syntax.git
git clone https://github.com/othree/html5.vim.git
git clone https://github.com/tpope/vim-git.git
git clone https://github.com/tpope/vim-markdown.git
git clone https://github.com/zaiste/tmux.vim.git
# Vim packages
git clone https://github.com/vim-scripts/IndexedSearch.git

# Generate help files
vim -c 'Helptags' -c 'qa!'
