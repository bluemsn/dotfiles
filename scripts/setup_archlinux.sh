#!/bin/zsh
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

pacman -S xf86-input-synaptics


# WINDOW MANAGER / DESKTOP ENVIRONMENT / ENVIRONMENT

pacman -S awesome
pacman -S xorg-server-xephyr

pacman -S dmenu && packer -S dmenu-launch
pacman -S xbindkeys
pacman -S conky
pacman -S ranger
pacman -S libcaca highlight atool lynx poppler transmission-cli mediainfo
#pacman -S thunar
#pacman -S file-roller unrar zip p7zip arj unace unzip
#pacman -S thunar-archive-plugin thunar-media-tags-plugin ffmpegthumbnailer tumbler


# CODE / DEV ENVIRONMENT

pacman -S zsh
chsh -s /bin/zsh eduan && chsh -s /bin/zsh
pacman -S git
pacman -S mercurial

pacman -S python2 python
#pacman -S markdown

pacman -S vim
packer -S sublime-text
packer -S sublime-text-nightly # ST3 dev
packer -S terminology-git
pacman -S rxvt-unicode
#pacman -S konsole
#pacman -S yakuake
#git clone https://github.com/zealot128/yakuake-solarized.git ~/yakuake-solarized && sh ~/yakuake-solarized/install.sh && rm ~/yakuake-solarized


# NETWORK & INTERNET

packer -S qbittorrent
packer -S filezilla
packer -S dropbox
pacman -S openssh
#pacman -S ddclient
#pacman -S irssi
#packer -S centerim

##  Web Browsers

pacman -S firefox
pacman -S chromium
pacman -S opera

## Local Dev

pacman -S apache php php-apache mariadb
systemctl enable httpd
systemctl start htppd
#packer -S xampp

## Network Manager / Wifi support

#pacman -S networkmanager network-manager-applet
pacman -S wpa_supplicant bluez bluez-utils
pacman -s connman
systemctl enable connman
systemctl enable bluetooth
systemctl start connman
systemctl start bluetooth
packer -S econnman


# MEDIA & ENTERTAINMENT

pacman -S flashplugin lib32-flashplugin
pacman -S gimp
#packer -S acroread
#pacman -S recordmydesktop gtk-recordmydesktop jack
pacman -S vlc
#packer -S spotify ffmpeg-spotify
#packer -S potamux
#pacman -S steam libtxc_dxtn lib32-libtxc_dxtn lib32-intel-dri


# FONTS

packer -S ttf-dejavu ttf-source-code-pro ttf-anonymous-pro terminus-font-ttf
packer -S ttf-tahoma ttf-symbola
packer -S ttf-google-fonts-git ttf-ms-fonts ttf-vista-fonts ttf-mac-fonts
fc-cache -vf


# MISCELLANEOUS

pacman -S acpi
packer -S par
#pacman -S python2-pip
#pip2 install parsedatetime
#pip2 install jrnl
pacman -S xclip
packer -S redshift

packer -S nodejs
npm install -g docpad
