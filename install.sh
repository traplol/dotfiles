#!/bin/bash

this_script=$0
dotfiles=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )
dotfiles_old=~/dotfiles_old

mkdir -p $dotfiles_old
cd $dotfiles


for f in "$dotfiles"/*
do
    dotfile=~/.${f##*/}
    bakup=$dotfiles_old/${dotfile##*/}
    t1=${f##*/}
    t2=${this_script##*/}
    # Skip this install script and git stuff
    if [ "$t1" = "$t2" ] || [ "$t1" = ".git*" ] ; then
        continue
    fi
    # Check if the dotfile already exists
    if [ \( -f $dotfile -o -d $dotfile \) ]; then
        # If the dotfile is not a symbolic link,
        if [ ! -L $dotfile ]; then
            # Backup the file in ~/dotfiles_old/
            echo "'$dotfile' exists; backing up in '$bakup'"
            mv $dotfile $bakup
        else
            echo "'$dotfile' is already a symlink, skipping."
        fi
    fi

    # Don't bother replacing an existing symlink
    if [ ! -L $dotfile ]; then
        echo "Creating symlink to '$f' at '$dotfile'"
        ln -s "$f" "$dotfile"
    fi

done

echo "Done."


