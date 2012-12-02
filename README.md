## How to get dotfiles

Simply find your dotfiles repo (I assume you have one, cause you should) and clone the master branch, or whatever the name of your master branch is. In my case I would run the following command:<br />
`$ git clone git://github.com/Greduan/dotfiles.git ~/dotfiles`

This command will clone my dotfiles repo and put it in the `dotfiles` repo in my home folder.

Or else go to your dotfiles repo and do what you want with it. Mine resides here: [GitHub > Greduan/dotfiles](https://github.com/Greduan/dotfiles)

BTW, you're already in my repo if you're reading this.

## Installing dotfiles

Installing, or setting up your dotfiles is very easy, just remember I'm using a Mac so my commands will probably be different from yours if you're not using a Mac.

Simply run the following commands in Terminal:<br />
`$ cd ~/dotfiles`<br />
`$ sh ./bootstrap.sh`

Which will run a script I made in order to make the symlinks, and other stuff, for you!
