#!/bin/zsh
# =============================================================================
# set_osx_defaults.sh
# -----------------------------------------------------------------------------
# This file will set stuff like I like it in Mac OS X, like show hidden files
# and other useful and important stuff.
#
# Credits to @skwp, he gave me many of these ideas:
# https://github.com/skwp/dotfiles/blob/master/osx
# =============================================================================
# Eduan Lavaque <eduan@snapsimpletech.com>
# Licensed under the MIT license (htpp://eduan.mit-license.org/)
# =============================================================================

# Dock settings:

# To have the 2D Dock instead of the fancy one.
defaults write com.apple.dock no-glass -bool true

# Remove the pop-out and pop-in delay for Dock.
defaults write com.apple.Dock autohide-delay -float 0

# Make hidden applications transparent in Dock.
defaults write com.apple.Dock showhidden -bool true


# Keyboard settings:

# Disable press-and-hold for keys in favor of key repeat. Requires restart.
defaults write -g ApplePressAndHoldEnabled -bool false

# Set a really fast key repeat.
defaults write NSGlobalDomain KeyRepeat -int 0

# Enable full keyboard access for all controls (e.g. enable Tab in modal
# dialogs).
defaults write NSGlobalDomain AppleKeyboardUIMode -int 3


# Finder settings:

# Always open everything in Finder's list view. This is important.
defaults write com.apple.Finder FXPreferredViewStyle Nlsv

# Always show all hidden files in Finder.
defaults write com.apple.Finder AppleShowAllFiles -bool true

# Set the Finder prefs for showing a few different volumes on the Desktop.
defaults write com.apple.finder ShowExternalHardDrivesOnDesktop -bool true
defaults write com.apple.finder ShowRemovableMediaOnDesktop -bool true

# Show all filename extensions in Finder.
defaults write com.apple.finder AppleShowAllExtensions -bool true

# Allow text selection in Quick Look.
defaults write com.apple.finder QLEnableTextSelection -bool true

# Don't write .DS_Store files on network volumes, hallelujah!
defaults write com.apple.desktopservices DSDontWriteNetworkStores -bool true

# Show the ~/Library folder.
chflags nohidden ~/Library/


# Spotlight settings:

# Disable Spotlight indexing.
sudo mdutil -a -i off

# Hide the Spotlight icon, since we're not gonna use it.
sudo chmod 600 /System/Library/CoreServices/Search.bundle/Contents/MacOS/Search


# Random settings:

# Show system info at login screen.
sudo defaults write /Library/Preferences/com.apple.loginwindow AdminHostInfo HostName

# Always show scrollbars.
defaults write NSGlobalDomain AppleShowScrollBars -string "Always"


# Dialogs and warnings settings:

# Disable warning for changing file type through the name.
defaults write com.apple.finder FXEnableExtensionChangeWarning -bool false

# Expand save panel by default.
defaults write NSGlobalDomain NSNavPanelExpandedStateForSaveMode -bool true

# Disable the "Are you sure you want to open this application?" dialog.
defaults write com.apple.LaunchServices LSQuarantine -bool false


# Trackpad and mouse settings:

# Enable tap to click (Trackpad) for this user and for the login screen.
defaults write com.apple.driver.AppleBluetoothMultitouch.trackpad Clicking -bool true
defaults -currentHost write NSGlobalDomain com.apple.mouse.tapBehavior -int 1
defaults write NSGlobalDomain com.apple.mouse.tapBehavior -int 1


# Finishing off...

# Restart all the necessary processes.
killall Finder
killall Dock
killall -HUP SystemUIServer

# Show done message.
echo 'Done... Expect some serious, momentary lag. Mostly from Finder and maybe the Terminal app.'
echo 'Also, many of these settings require a restart to take place, so I recommend you restart now.'
