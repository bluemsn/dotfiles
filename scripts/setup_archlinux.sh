#!/usr/bin/zsh
# =============================================================================
# setup_archlinux.sh
# -----------------------------------------------------------------------------
# This file is meant to install stuff that doesn't come pre-installed with Arch
# Linux, but that you do need. Also stuff that you didn't install during the
# setup process...
# =============================================================================
# Eduan Lavaque <eduan@snapsimpletech.com>
# Licensed under the MIT license (http://eduan.mit-license.org/)
# =============================================================================

# DRIVERS / HARDWARE

sudo pacman -S xf86-input-synaptics
libtxc_dxtn lib32-libtxc_dxtn lib32-intel-dri


# WINDOW MANAGER / DESKTOP ENVIRONMENT / ENVIRONMENT

packer -S alopex-git
packer -S interrobang-git
sudo pacman -S bash-completion
sudo pacman -S xorg-server-xephyr

sudo pacman -S dmenu && packer -S dmenu-launch
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
packer -S sublime-text
packer -S sublime-text-nightly # ST3 dev
packer -S terminology-git
sudo pacman -S rxvt-unicode
#sudo pacman -S konsole
#sudo pacman -S yakuake
#git clone https://github.com/zealot128/yakuake-solarized.git ~/yakuake-solarized && sh ~/yakuake-solarized/install.sh && rm ~/yakuake-solarized


# NETWORK & INTERNET

packer -S qbittorrent
packer -S filezilla
packer -S dropbox
sudo pacman -S openssh
#sudo pacman -S ddclient
#sudo pacman -S irssi
#packer -S centerim

##  Web Browsers

sudo pacman -S firefox
sudo pacman -S chromium
sudo pacman -S opera

## Local Dev

sudo pacman -S apache php php-apache mariadb
systemctl enable httpd
systemctl start htppd
#packer -S xampp

## Network Manager / Wifi support

#sudo pacman -S networkmanager network-manager-applet
sudo pacman -S wpa_supplicant bluez bluez-utils
sudo pacman -s connman
systemctl enable connman
systemctl enable bluetooth
systemctl start connman
systemctl start bluetooth
packer -S econnman


# MEDIA & ENTERTAINMENT

sudo pacman -S flashplugin lib32-flashplugin
sudo pacman -S gimp
#packer -S acroread
#sudo pacman -S recordmydesktop gtk-recordmydesktop jack
sudo pacman -S vlc
#packer -S spotify ffmpeg-spotify
#packer -S potamux
#
## GAMES
sudo pacman -S steam
packer -S desurium
packer -S dwarffortress-ironhand


# FONTS

packer -S ttf-dejavu ttf-source-code-pro ttf-anonymous-pro terminus-font-ttf
packer -S ttf-tahoma ttf-symbola
packer -S ttf-google-fonts-git ttf-ms-fonts ttf-vista-fonts ttf-mac-fonts
fc-cache -vf


# MISCELLANEOUS

sudo pacman -S acpi
packer -S par
sudo pacman -S xclip
#packer -S redshift
#sudo pacman -S python2-pip
#pip2 install parsedatetime
#pip2 install jrnl

packer -S nodejs
npm install -g docpad
