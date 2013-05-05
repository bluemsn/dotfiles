#!/bin/zsh
# =============================================================================
# setup_archlinux.sh
# -----------------------------------------------------------------------------
# This file is meant to install stuff that doesn't come pre-installed with Arch
# Linux, but that you do need. Also stuff that you did or didn't install during
# the setup process...
# =============================================================================
# Eduan Lavaque <eduan@snapsimpletech.com>
# Licensed under the MIT license (http://eduan.mit-license.org/)
# =============================================================================

packer -S ttf-dejavu
packer -S ttf-tahoma
packer -S ttf-symbola
packer -S ttf-google-webfonts-git ttf-ms-fonts ttf-vista-fonts ttf-mac-fonts
fc-cache -vf

pacman -S englightenment17
#pacman -S networkmanager network-manager-applet
pacman -s connman
packer -S econnman
systemctl enable connman
systemctl start connman
pacman -S ddclient

pacman -S firefox
pacman -S chromium
pacman -S flashplugin lib32-flashplugin
packer -S filezilla
packer -S dropbox
pacman -S gimp
packer -S acroread

pacman -S apache php php-apache mariadb
systemctl enable httpd
systemctl start htppd
#packer -S xampp

pacman -S python2 python
pacman -S python2-pip
pip2 install sh
pacman -S zsh
chsh -s /bin/zsh && sudo chsh -s /bin/zsh
pacman -S acpi
pacman -S git
pacman -S mercurial
pacman -S gvim
pacman -S markdown

pacman -S irssi
packer -S centerim
pacman -S unzip
packer -S par
pip2 install parsedatetime
pip2 install jrnl

#pacman -S steam libtxc_dxtn lib32-libtxc_dxtn lib32-intel-dri
