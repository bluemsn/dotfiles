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

if [ `uname -s` != 'Darwin' ]
	echo "You can't run this script, you're not using a Mac"
else
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


	# Finder settings:

	# Always open everything in Finder's list view. This is important.
	defaults write com.apple.Finder FXPreferredViewStyle Nlsv

	# Always show all hidden files in Finder.
	defaults write com.apple.Finder AppleShowAllFiles -bool true

	# Set the Finder prefs for showing a few different volumes on the Desktop.
	defaults write com.apple.finder ShowExternalHardDrivesOnDesktop -bool true
	defaults write com.apple.finder ShowRemovableMediaOnDesktop -bool true


	# Spotlight settings:

	# Disable Spotlight indexing.
	sudo mdutil -a -i off

	# Hide the Spotlight icon, since we're not gonna use it.
	sudo chmod 600 /System/Library/CoreServices/Search.bundle/Contents/MacOS/Search


	# Random settings:

	# Show system info at login screen.
	sudo defaults write /Library/Preferences/com.apple.loginwindow AdminHostInfo HostName

	# Don't write .DS_Store files, hallelujah!
	defaults write com.apple.desktopservices DSDontWriteNetworkStores -bool true

	# Show the ~/Library folder.
	chflags nohidden ~/Library/

	# Disable warning for changing file type through the name.
	defaults write com.apple.finder FXEnableExtensionChangeWarning -bool false


	# Finishing off...

	# Restart all the necessary processes.
	killall Finder
	killall Dock
	killall -HUP SystemUIServer

	# Show done message.
	echo 'Done... Expect some serious, momentary lag. Mostly from Finder and maybe the Terminal app.'
fi
