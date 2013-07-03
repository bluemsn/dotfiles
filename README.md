# Welcome to my dotfiles!

Welcome to my dotfiles repo! This is one of those repos in between thousands that are named the same, however have different contents.

This repo contains lots of customizations for [Vim][1] (not `vi` mind you), [Zsh][2], [Bash][3], [tmux][4], [Git][5] and lots of Linux software. I am running [Arch Linux][6] as my main OS.

I don't suggest you use these dotfiles as-is, because these are optimized for me, and I'm using a Mac Mini (2011) and a Dell Vostro 1500 laptop as well, both running Arch Linux. I suggest you simply glance through them or look for a solution in them, and if you find something that looks nice then just implement it in *your* dotfiles, made to work for *you*.

I don't suggest you just grab a  file and copy it over to your dotfiles, in fact I really discourage this. I suggest instead you read the file and grab what you like.

## How to get my dotfiles

In order to obtain my awesome dotfiles you can run the following command in your command line:

    $ git clone git://github.com/Greduan/dotfiles.git ~/dotfiles

This command will clone my dotfiles repo and put it in the `dotfiles` repo/folder in your home folder.

**Word of warning:** My dotfiles have been optimized fully for Arch Linux, so it's probable that some things don't work for you...

Installing, or setting up your dotfiles is very easy. Simply run the following commands in Terminal:

    $ cd ~/dotfiles
    $ sh ./scripts/symlinks.sh

Which will run a script I made in order to make the symlinks, and other stuff, for you! This will not make a backup of anything, so if you do need backups, make them before running this script.

## Contributing

If you ever feel like helping me out here, or you think that something can be done a better way, just open a new issue! Open a new issue expressing what I'm doing and what you think I should be doing.

However don't say "You shouldn't be using this colorscheme, use this one instead!" for example.

I mean helping out with the stuff that can be improved, for example how I can improve the logic for one of my Vim functions. Or one of my settings, how I can make them more compatible across more versions of the same software or with several OSs (although I only use Arch Linux) etc. Or how one setting/method is preferred over another because *blah*.

Now, you can open an issue expressing your ideas, or you can just fork this project and make the suggested changes, then just make a pull request with your changes. I will then check your changes, give comments if necessary, modify it if I find it necessary, and then merge it, or just close the pull request if I don't like the change.


  [1]: http://www.vim.org/
  [2]: http://es.wikipedia.org/wiki/Zsh
  [3]: http://es.wikipedia.org/wiki/Bash
  [4]: http://tmux.sourceforge.net/
  [5]: http://git-scm.com/
  [6]: https://www.archlinux.org/
