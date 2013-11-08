# Welcome to my dotfiles!

Welcome to my dotfiles repo! This is one of those repos in between thousands that are named the same, however will always have different contents. This is my take on dotfiles, take what you like and ignore what you don't like.

This repo contains lots of customizations for [Vim][1] (not `vi` mind you), [GNU Emacs](http://www.gnu.org/software/emacs/), [Zsh][2], [tmux][4], [Git][5] and lots of GNU/Linux software. I am running [Arch Linux][6] as my main OS in a Vostro 1500 laptop. I am running Mac OS X in a Mac Mini 2011 as well.

I don't suggest you just grab a  file and copy it over to your dotfiles, in fact I really discourage this. I suggest instead you read the file and grab what you like. It's bad practice to copy a file verbatim.

## How to get my dotfiles

In order to obtain my awesome dotfiles you can run the following command in your command line:

    $ git clone git://github.com/Greduan/dotfiles.git ~/dotfiles

This command will clone my dotfiles repo and put it in the `dotfiles` repo/folder in your home folder.

**Word of warning:** My dotfiles have been optimized fully for Arch Linux and Mac OS X, so it's very possible that some things won't work for you...

Installing, or setting up your dotfiles is very easy. Simply run the following command in the CLI:

    $ sh ~/dotfiles/scripts/symlinks.sh

Which will run a script I made in order to make the symlinks, and other stuff, for you! This will not make a backup of anything, so if you do need backups, make them before running this script.

## Contributing

If you ever feel like helping me out here, or you think that something can be done a better way, just open a new issue! Open a new issue expressing what I'm doing and what you think I should be doing.

However please don't oppen issues/pull requests for stuff that's completely up to opinion, like a color scheme for example. Instead open these for stuff that could be improved like function logic, in Vim something like how to make Vim more cross-compatible with different OS's etc.

Now, you can open an issue expressing your ideas, or you can just fork this project and make the suggested changes, then just make a pull request with your changes. I will then check your changes, give comments if necessary, modify it if I find it necessary, and then merge it, or just close the pull request if I don't like the change.

  [1]: http://www.vim.org/
  [2]: http://es.wikipedia.org/wiki/Zsh
  [3]: http://es.wikipedia.org/wiki/Bash
  [4]: http://tmux.sourceforge.net/
  [5]: http://git-scm.com/
  [6]: https://www.archlinux.org/

