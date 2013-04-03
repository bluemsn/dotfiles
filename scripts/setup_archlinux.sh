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

# Update Arch Linux
pacman -Syu
# In case `pacman` was upgraded...
pacman -Syu

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
chsh -s $(which zsh)
sudo chsh -s $(which zsh)

# VCS
pacman -S git
pacman -S mercurial

# Install (G)Vim
pacman -S gvim

# Install development/monospaced fonts
pacman -S ttf-dejavu
pacman -S ttf-monaco
pacman -S ttf-inconsolata
pacman -S ttf-inconsolata-g
# Variable width fonts
pacman -S ttf-tahoma
pacman -S ttf-symbola
# Font packages
pacman -S ttf-google-webfonts
pacman -S ttf-google-webfonts-hg
pacman -S ttf-ms-fonts
pacman -S ttf-vista-fonts
pacman -S ttf-mac-fonts