#!/bin/sh
# DRIVERS / HARDWARE

sudo pacman -S xf86-input-synaptics libtxc_dxtn


# WINDOW MANAGER / DESKTOP ENVIRONMENT / ENVIRONMENT

pacaur -S alopex-git
pacaur -S interrobang-git
sudo pacman -S bash-completion
sudo pacman -S xorg-server-xephyr

sudo pacman -S dmenu && pacaur -S dmenu-launch
sudo pacman -S xbindkeys
sudo pacman -S conky
sudo pacman -S ranger
sudo pacman -S libcaca highlight atool lynx poppler transmission-cli mediainfo
#sudo pacman -S thunar
#sudo pacman -S file-roller unrar zip p7zip arj unace unzip
#sudo pacman -S thunar-archive-plugin thunar-media-tags-plugin ffmpegthumbnailer tumbler


# CODE / DEV ENVIRONMENT

sudo pacman -S zsh
chsh -s /bin/zsh && sudo chsh -s /bin/zsh
sudo pacman -S git
sudo pacman -S mercurial

sudo pacman -S python2 python
#sudo pacman -S markdown

sudo pacman -S vim
sudo pacman -S tmux
pacaur -S sublime-text
pacaur -S sublime-text-nightly # ST3 dev
pacaur -S terminology-git
sudo pacman -S rxvt-unicode
#sudo pacman -S konsole
#sudo pacman -S yakuake
#git clone https://github.com/zealot128/yakuake-solarized.git ~/yakuake-solarized && sh ~/yakuake-solarized/install.sh && rm ~/yakuake-solarized


# NETWORK & INTERNET

pacaur -S qbittorrent
pacaur -S filezilla
pacaur -S dropbox
sudo pacman -S openssh
#sudo pacman -S ddclient
#sudo pacman -S irssi
#pacaur -S centerim

##  Web Browsers

sudo pacman -S firefox
sudo pacman -S chromium
sudo pacman -S opera

## Local Dev

sudo pacman -S apache php php-apache mariadb
systemctl enable httpd
systemctl start htppd
#pacaur -S xampp

## Network Manager / Wifi support

#sudo pacman -S networkmanager network-manager-applet
sudo pacman -S wpa_supplicant bluez bluez-utils
sudo pacman -s connman
systemctl enable connman
systemctl enable bluetooth
systemctl start connman
systemctl start bluetooth
pacaur -S econnman


# MEDIA & ENTERTAINMENT

sudo pacman -S flashplugin lib32-flashplugin
sudo pacman -S gimp
#pacaur -S acroread
#sudo pacman -S recordmydesktop gtk-recordmydesktop jack
sudo pacman -S vlc
#pacaur -S spotify ffmpeg-spotify
#pacaur -S potamux
#
## GAMES
sudo pacman -S steam
pacaur -S desurium
pacaur -S dwarffortress-ironhand


# FONTS

pacaur -S ttf-dejavu ttf-source-code-pro ttf-anonymous-pro terminus-font-ttf
pacaur -S ttf-tahoma ttf-symbola
pacaur -S ttf-google-fonts-git ttf-ms-fonts ttf-vista-fonts ttf-mac-fonts
fc-cache -vf


# MISCELLANEOUS

sudo pacman -S acpi
pacaur -S par
sudo pacman -S xclip
pacaur -S xflux

#sudo pacman -S python2-pip
#pip2 install parsedatetime
#pip2 install jrnl

pacaur -S nodejs
sudo npm install -g docpad
sudo npm install -g grunt-cli
