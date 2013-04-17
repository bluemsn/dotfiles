# Welcome to my dotfiles!

Welcome to my dotfiles repo! This is one of those repos in between thousands that are named the same, however have different contents. This one is very opinionated so if you think something can be done a better way just create an issue with your opinion, I will consider it and implement it if I like it.

This repo contains lots of customizations for Vim (not `vi` mind you), Zsh, Bash, tmux, Git etc., all of which are opinionated by me, and maybe will have help from other people (not yet though).

I don't suggest you use these dotfiles as-is, because these are optimized for me, and I'm using a Mac Mini with Mac OSX Mountain Lion. I suggest you simply glance through them or look for a solution in them, and if you find something that looks nice then just implement it in *your* dotfiles, made to work for *you*.

**I GREATLY ENCOURAGE COPYING!** I discourage mass copying though, where you just grab the entire file and don't edit it at all. However I greatly encourage reading through it and finding what you like and implementing it.

## How to get dotfiles

In order to obtain my awesome dotfiles you can run the following command in your command line:
```console
$ git clone git://github.com/Greduan/dotfiles.git ~/dotfiles
```

This command will clone my dotfiles repo and put it in the `dotfiles` repo/folder in your home folder.

**Word of warning:** This will only work on a Mac, it has only been tested/used in a Mac Mini with OSX Mountain Lion. However the symlink maker/remover will also work with Linux. What I mean is that the settings are not sure to work with something also than a Mac.

## Installing dotfiles

Installing, or setting up your dotfiles is very easy, just remember I'm using a Mac so my commands will probably be different from yours if you're not using a Mac.

Simply run the following commands in Terminal:
```console
$ cd ~/dotfiles
$ python2 ./scripts/symlinks.py
```

Which will run a script I made in order to make the symlinks, and other stuff, for you! Make a backup of any files that count as `*rc` files, cause they're gonna be deleted, if they're symlinks it doesn't matter though.

## Contributing

If you ever feel like helping me out here, or you think that something can be done a better way, just open a new issue! Open a new issue expressing what I'm doing and what you think I should be doing.

However don't say "You shouldn't be using this colorscheme, use this one instead!" for example.

I mean helping out with the stuff that can be improved, for example how I can improve the logic for one of my Vim functions. Or one of my settings, how I can make them more compatible across more versions of the same software etc. Or how one setting/method is preferred over another because *blah*.

Now, you can open an issue expressing your ideas, or you can just [fork][1] this project and make the suggested changes, then just make a pull request with your changes. I will then check your changes, give comments if necessary, modify it if I find it necessary, and then merge it, or just tell you that I don't like that change (like a cruel man) and close the pull request.

[1]: https://github.com/Greduan/dotfiles/fork_select
