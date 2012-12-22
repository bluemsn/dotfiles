## How to get dotfiles

In order to obtain my awesome dotfiles you can run the following command in the Mac Terminal application (or your preffered Terminal).
```console
$ cd ~
$ git clone git://github.com/Greduan/dotfiles.git ~/dotfiles
```

This command will clone my dotfiles repo and put it in the `dotfiles` repo in your home folder.

**Word of warning:** This will only work on a Mac, it has only been tested/used in a Mac Mini with OSX Mountain Lion. However the symlink maker/remover will also work with Linux. What I mean is that the settings are not sure to work with something also than a Mac.

## Installing dotfiles

Installing, or setting up your dotfiles is very easy, just remember I'm using a Mac so my commands will probably be different from yours if you're not using a Mac.

Simply run the following commands in Terminal:
```console
$ cd ~/dotfiles
$ sh ./bootstrap.sh
```

Which will run a script I made in order to make the symlinks, and other stuff, for you!
