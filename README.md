## How to get dotfiles

Simply go to your own dotfiles repo (if you have one) and download your files. I use my own so I download the following zip file, which contains my master branch: https://nodeload.github.com/Greduan/dotfiles/zip/master

Once I've done that unzip it in your home folder (`~`) and rename it to `.dotfiles`.

Or else go to your dotfiles repo and do what you want with it. Mine resides here: [GitHub > Greduan/dotfiles](https://github.com/Greduan/dotfiles)

BTW, you're already in my repo if you're reading this.

## Installing dotfiles

Installing, or setting up your dotfiles is very easy, just remember I'm using a Mac so my commands will probably be different from yours if you're not using a Mac.

Simply run the following commands in Terminal:

### Vim

`$ ln -s ~/.dotfiles/vim ~/.vim`<br />
`$ ln -s ~/.dotfiles/vim/vimrc ~/.vimrc`<br />
`$ ln -s ~/.dotfiles/vim/gvimrc ~/.gvimrc`

### Zsh

`$ ln -s ~/.dotfiles/zsh ~/.zsh`<br />
`$ ln -s ~/.dotfiles/zsh/zshrc ~/.zshrc`

### Git

`$ ln -s ~/.dotfiles/git/gitconfig ~/.gitconfig`<br />
`$ ln -s ~/.dotfiles/git/gitmessage.txt ~/.gitmessage.txt`
