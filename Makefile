# This Makefile only works on Arch Linux
symlink() {
	ln -fsv $HOME/dotfiles/$1 $HOME/$2
}

bin:
	mkdir -p $HOME/bin $HOME/tasks
	hg clone https://bitbucket.org/sjl/t ~/bin/t
	git clone https://github.com/rupa/z.git ~/bin/z

submodule:
	git clone https://github.com/cask/cask.git emacs.d/cask

pip:
	sudo pacman -S python-pip
	sudo pip install jrnl
	sudo pip install d

npm:
	sudo pacman -S nodejs
	sudo npm install -g coffee-script
	sudo npm install -g docpad
	sudo npm install -g grunt-cli
	sudo npm install -g yo
	sudo npm install -g generator-docpad

git:
	git config --global user.name "Greduan"
	git config --global user.email "eduanlavaque@gmail.com"
	git config --global core.editor vim
	git config --global core.pager 'less -r'
	git config --global help.autocorrect 1
	git config --global color.ui true
	git config --global core.eol lf
	git config --global core.autocrlf input
	git config --global alias.logg 'log --graph --decorate --oneline --abbrev-commit --all'
	git config --global alias.root 'rev-parse --show-toplevel'
	git config --global push.default simple

symlink:
	mkdir -p $HOME/.config
	if [ `uname -s` == 'Linux' ]; then
		symlink config/alopex .config/alopex
		symlink config/redshift.conf .config/redshift.conf
		symlink conkyrc       .conkyrc
		symlink fuzzy-windows .fuzzy-windows
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
	symlink gitignore_global .gitignore_global
	symlink gvimrc       .gvimrc
	symlink hgignore     .hgignore
	symlink hgrc         .hgrc
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
	if [ `uname -s` == 'Linux' ]; then
		ln -fsv $HOME/Dropbox/Sublime\ Text\ 3/Packages/User $HOME/.config/sublime-text-3/Packages/User
	elif [ `uname -s` == 'Darwin' ]; then
		ln -fsv $HOME/Dropbox/Sublime\ Text\ 3/Packages/User $HOME/Library/Application\ Support/Sublime\ Text\ 3/Packages/User
	fi

pacman:
	sudo pacman -S xf86-input-synaptics libtxc_dxtn
	sudo pacman -S xorg-server-xephyr
	sudo pacman -S dmenu
	sudo pacman -S xbindkeys
	sudo pacman -S zsh; chsh -s /bin/zsh && sudo chsh -s /bin/zsh
	sudo pacman -S git
	sudo pacman -S python2 python
	sudo pacman -S gvim
	sudo pacman -S tmux
	sudo pacman -S filezilla
	sudo pacman -S openssh
	sudo pacman -S firefox
	sudo pacman -S chromium
	sudo pacman -S opera
	sudo pacman -S wpa_supplicant bluez bluez-utils; sudo systemctl enable bluetooth; sudo systemctl start bluetooth;
	sudo pacman -S connman; sudo systemctl enable connman; sudo systemctl start connman
	sudo pacman -S flashplugin lib32-flashplugin
	sudo pacman -S gimp
	sudo pacman -S vlc
	sudo pacman -S acpi
	sudo pacman -S xclip
	sudo pacman -S imagemagick
	sudo pacman -S tree
	sudo pacman -S libreoffice
	sudo pacman -S redshift
	sudo pacman -S xmonad
	sudo pacman -S xmonad-contrib
	sudo pacman -S xmobar
	sudo pacman -S trayer
	sudo pacman -S simplescreenrecorder
	sudo pacman -S transmission-qt
	sudo pacman -S kdenlive
	sudo pacman -S mercurial
	sudo pacman -S ranger

pacaur:
	pacaur -Sa dmenu-launch
	pacaur -Sa rxvt-unicode-256xresources
	pacaur -Sa dropbox
	pacaur -Sa econnman
	pacaur -Sa ttf-dejavu ttf-source-code-pro ttf-anonymous-pro terminus-font-ttf
	pacaur -Sa ttf-tahoma ttf-symbola
	pacaur -Sa ttf-google-fonts-git ttf-ms-fonts ttf-vista-fonts ttf-mac-fonts
	fc-cache -vf
	pacaur -Sa par
	pacaur -Sa leiningen
	pacaur -Sa ldm; sudo systemctl enable ldm; sudo systemctl start ldm
	pacaur -Sa ngrok
	pacaur -Sa most
	pacaur -Sa z-git
	pacaur -Sa workman-git

arch: pacman pacaur

vim:
	mkdir -pv ~/.vim/tmp/{backup,swap,undo,unite}
