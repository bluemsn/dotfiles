#!/bin/sh
# Simple script to run in order to update all the plugins
find . -maxdepth 1 -type d -exec sh -c '(cd {} && git pull)' ';'

# for future use...
#mkdir -pv ~/.vim/tmp/{backup,swap,undo}
#mkdir -pv ~/.vim/{autoload,bundle}
