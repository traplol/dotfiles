# my dotfiles
This is my personal setup of dotfiles I use on my workstations.

## install
The installation process should backup any existing dotfiles that will be replaced in ``` dotfiles/backup ```.
```
$ git clone https://github.com/traplol/dotfiles.git
$ cd dotfiles
$ ./install.sh -c
```
## options
see: ``` $ dotfiles/install.sh --help ``` for a full list of options
```
-c --clone-submodules       Clones any git submodules
-n --no-link                Doesn't create any sym links to the dotfiles, effectively this doesn't install.
-u --update                 Checks for updates.
-g --generate               Creates a directory and copies the boostrap installer to the new directory.

--uninstall                 Restores backups that were automatically generated.
```

## using ``` -g --generate ```
Using ``` $ dotfiles/install.sh -g DIRECTORY_NAME ``` will generate a directory called ```DIRECTORY_NAME``` and
copy ```bootstrap.sh.template``` there as ```bootstrap.sh.template```. You should never need to run this script
as it is automatically called by running ```dotfiles/install.sh```. Once the directory has been created just copy
any ```.dotfiles``` or ```.dotfolders``` here without the preceding dot.

Ex:
```
.vim -> vim
.vimrc -> vimrc
.tmux -> tmux
```
