#!/bin/zsh
# =============================================================================
# setup_archlinux.sh
# -----------------------------------------------------------------------------
# This file is meant to install stuff that the following script didn't
# install/setup: https://github.com/helmuthdu/aui
# =============================================================================
# Eduan Lavaque <eduan@snapsimpletech.com>
# Licensed under the MIT license (http://eduan.mit-license.org/)
# =============================================================================

# AUR Packages
##############

packer -S dropbox
packer -S centerim

packer -S ttf-dejavu
packer -S ttf-tahoma
packer -S ttf-symbola
packer -S ttf-google-webfonts-git
packer -S ttf-ms-fonts
packer -S ttf-vista-fonts
packer -S ttf-mac-fonts

# Official Packages
###################

pacman -S openbox
pacman -S tint2

pacman -S firefox
pacman -S chromium
pacman -S flashplugin
pacman -S lib32-flashplugin

pacman -S python2
pacman -S python

pacman -S zsh
chsh -s /bin/zsh
sudo chsh -s /bin/zsh

pacman -S git
pacman -S mercurial

pacman -S gvim

pacman -S apache php php-apache mariadb

pacman -S irssi
pacman -S mupdf
pacman -S unzip
