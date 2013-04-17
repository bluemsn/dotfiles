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

# This will stop working on 2017
pacman -S flashplugin

# Install my development environment
####################################

# Python
pacman -S python2
pacman -S python

# Install Zsh
pacman -S zsh
# Set Zsh as default shell
chsh -s /bin/zsh
sudo chsh -s /bin/zsh

# VCS
pacman -S git
pacman -S mercurial

# Install (G)Vim
pacman -S gvim

# Install development/monospaced fonts
packer -S ttf-dejavu
# Variable width fonts
packer -S ttf-tahoma
packer -S ttf-symbola
# Font packages
packer -S ttf-google-webfonts-git
packer -S ttf-ms-fonts
packer -S ttf-vista-fonts
packer -S ttf-mac-fonts


