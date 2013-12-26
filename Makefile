all:
	# Clone the repos, instead of Git modules
	git clone https://github.com/yonchu/zsh-vcs-prompt.git zsh/zsh-vcs-prompt
	git clone https://github.com/zsh-users/zsh-syntax-highlighting.git zsh/fish-highlighting
	git clone https://github.com/cask/cask.git emacs.d/cask
	git clone https://github.com/tomtom/tplugin_vim.git vim/bundles/tplugin_vim

git:
	git config --global user.name "Greduan"
	git config --global user.email "eduanlavaque@gmail.com"
	git config --global core.editor vim
	#git config --global commit.template $HOME/.gitmessage.txt
	git config --global core.pager 'less -r'
	git config --global help.autocorrect 1
	git config --global color.ui true
	git config --global core.eol lf
	git config --global core.autocrlf input
	git config --global alias.logg 'log --graph --decorate --oneline --abbrev-commit --all'
	git config --global alias.root 'rev-parse --show-toplevel'
	git config --global push.default simple

vim:
	mkdir -pv ~/.vim/tmp/{backup,swap,undo}
	mkdir -pv ~/.vim/{autoload,bundle}
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
	git clone https://github.com/tpope/vim-vinegar.git
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

symlink:
	symlink() {
		ln -fsv $HOME/dotfiles/$1 $HOME/$2
	}
	mkdir -p $HOME/.config
	# dotfiles
	if [ `uname -s` == 'Linux' ]; then
		symlink config/alopex .config/alopex
		symlink conkyrc       .conkyrc
		symlink xbindkeysrc   .xbindkeysrc
		symlink xinitrc       .xinitrc
		symlink xmobarrc      .xmobarrc
		symlink xmonad        .xmonad
	fi
	symlink Xdefaults    .Xdefaults
	symlink bin          bin
	symlink curlrc       .curlrc
	symlink emacs.d      .emacs.d
	symlink gitconfig    .gitconfig
	symlink inputrc      .inputrc
	symlink irssi        .irssi
	symlink zsh          .zsh
	symlink zshrc        .zshrc
	symlink profile      .profile
	symlink tmux.conf    .tmux.conf
	symlink vim          .vim
	symlink vimrc        .vimrc
	symlink editorconfig .editorconfig
	symlink config/fish  .config/fish
	# Sublime Text
	if [ `uname -s` == 'Linux' ]; then
		ln -fsv $HOME/Dropbox/Sublime\ Text\ 3/Packages $HOME/.config/sublime-text-3/Packages
		ln -fsv $HOME/Dropbox/Sublime\ Text\ 3/Installed\ Packages $HOME/.config/sublime-text-3/Installed\ Packages
	elif [ `uname -s` == 'Darwin' ]; then
		ln -fsv $HOME/Dropbox/Sublime\ Text\ 3/Packages $HOME/Library/Application\ Support/Sublime\ Text\ 3/Packages
		ln -fsv $HOME/Dropbox/Sublime\ Text\ 3/Installed\ Packages $HOME/Library/Application\ Support/Sublime\ Text\ 3/Installed\ Packages
	fi
