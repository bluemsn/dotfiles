#!/bin/zsh
# =============================================================================
# set_osx_defaults.sh
# -----------------------------------------------------------------------------
# This file will set stuff like I like it in Mac OS X, like show hidden files
# and other useful and important stuff.
# =============================================================================
# Eduan Lavaque <eduan@snapsimpletech.com>
# Licensed under the MIT license (htpp://eduan.mit-license.org/)
# =============================================================================

# Disable press-and-hold for keys in favor of key repeat.
defaults write -g ApplePressAndHoldEnabled -bool false

# Always open everything in Finder's list view. This is important.
defaults write com.apple.Finder FXPreferredViewStyle Nlsv

# Always show all hidden files in Finder
defaults write com.apple.Finder AppleShowAllFiles -bool true
killall Finder

# Don't write .DS_Store files, hallelujah!
defaults write com.apple.desktopservices DSDontWriteNetworkStores -bool true

# To have the 2D Dock instead of the fancy one
defaults write com.apple.dock no-glass -bool true; killall Dock

# Disable Spotlight indexing
sudo mdutil -a -i off

# Hide the Spotlight icon
sudo chmod 600 /System/Library/CoreServices/Search.bundle/Contents/MacOS/Search
killall -HUP SystemUIServer

# Show the ~/Library folder.
chflags nohidden ~/Library

# Set a really fast key repeat.
defaults write NSGlobalDomain KeyRepeat -int 0

# Set the Finder prefs for showing a few different volumes on the Desktop.
defaults write com.apple.finder ShowExternalHardDrivesOnDesktop -bool true
defaults write com.apple.finder ShowRemovableMediaOnDesktop -bool true

echo 'Done... Expect some serious, momentary lag. Mostly fro Finder and maybe the Terminal app.'
