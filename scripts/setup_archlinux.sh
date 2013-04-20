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

# AUR Packages
##############

packer -S dropbox
packer -S centerim

packer -S ttf-dejavu
packer -S ttf-tahoma
packer -S ttf-symbola
packer -S ttf-google-webfonts-git ttf-ms-fonts ttf-vista-fonts ttf-mac-fonts

# Official Packages
###################

pacman -S xcompmgr
pacman -S transset-df

#pacman -S thunar thunar-archive-plugin thunar-media-tags-plugin tumbler ffmpegthumbnailer

pacman -S firefox
pacman -S chromium
pacman -S flashplugin lib32-flashplugin

pacman -S python2 python

pacman -S zsh
chsh -s /bin/zsh && sudo chsh -s /bin/zsh

pacman -S git
pacman -S mercurial

pacman -S gvim

pacman -S apache php php-apache mariadb

pacman -S irssi
pacman -S mupdf
pacman -S unzip
