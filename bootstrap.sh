#!/bin/zsh
# =============================================================================
# bootstrap.sh
# -----------------------------------------------------------------------------
# This script will run all the scripts in the best order.
# =============================================================================
# Eduan Lavaque <eduan@snapsimpletech.com>
# Licensed under the MIT license (htpp://eduan.mit-license.org/2012-2013)
# =============================================================================

kernel=`uname -s`  # Current Kernel name
time=`date +%H:%M` # Output current time
bootstrap=1        # Now scripts know they've been called by bootstrap file

echo "I'm gonna do some stuff for you, OK? I started doing stuff at [$time]..."

if [ $kernel == 'Darwin' ]; then
	# Set some OSX options
	source ./scripts/set_osx_defaults.sh
fi

# Make the necessary symlinks
source ./scripts/make_symlinks.sh

if [ $kernel == 'Darwin' ]; then
	# Install some Homebrew stuff
	source ./scripts/brew_installs.sh
fi

# Setup Vim for first use
source ./scripts/setup_vim.sh

# Set the default shell
chsh -s /bin/zsh
sudo chsh -s /bin/zsh

# Install all submodules
git submodule update --init

echo "All right! I finished! It's [$time] right now."
