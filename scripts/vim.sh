#!/bin/sh
# Make folders for tmp stuff
mkdir -pv ~/.vim/tmp/{backup,swap,undo}

# Make folders for Pathogen
mkdir -p ~/.vim/{autoload,bundle}

# Install Pathogen
curl -Sso ~/.vim/autoload/pathogen.vim https://raw.github.com/tpope/vim-pathogen/master/autoload/pathogen.vim

# Install all the plugins:
# General plugins...
#git clone https://github.com/kien/ctrlp.vim.git
git clone https://github.com/mattn/emmet-vim.git
git clone https://github.com/fholgado/minibufexpl.vim.git
git clone https://github.com/kien/rainbow_parentheses.vim.git
git clone https://github.com/tomtom/tcomment_vim.git
git clone https://github.com/Shougo/unite.vim.git
git clone https://github.com/ConradIrwin/vim-bracketed-paste.git
git clone https://github.com/gorodinskiy/vim-coloresque.git
git clone https://github.com/drmikehenry/vim-fixkey.git
git clone https://github.com/osyo-manga/vim-over.git
git clone https://github.com/chreekat/vim-paren-crosshairs.git
git clone https://github.com/tpope/vim-repeat.git
git clone https://github.com/justinmk/vim-sneak.git
git clone https://github.com/tpope/vim-surround.git
git clone https://github.com/christoomey/vim-tmux-navigator.git
git clone https://github.com/sjl/vitality.vim.git
# Color schemes...
git clone https://github.com/morhetz/gruvbox.git
git clone https://github.com/jnurmine/Zenburn.git
# Syntaxes...
git clone https://github.com/othree/html5.vim.git
git clone https://github.com/cakebaker/scss-syntax.vim.git
git clone https://github.com/zaiste/tmux.vim.git
git clone https://github.com/guns/vim-clojure-static.git
git clone https://github.com/kchmck/vim-coffee-script.git
git clone https://github.com/mutewinter/vim-css3-syntax.git
git clone https://github.com/AndrewRadev/vim-eco.git
git clone https://github.com/tpope/vim-git.git
git clone https://github.com/elzr/vim-json.git
git clone https://github.com/tpope/vim-markdown.git
# Vim packages
#git clone https://github.com/vim-scripts/Auto-Pairs.git
git clone https://github.com/vim-scripts/IndexedSearch.git

# Generate help files
vim -c 'Helptags' -c 'qa!'
