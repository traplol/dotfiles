#!/bin/bash
set -e

this_script=$0
this_script_args=$@

src="${BASH_SOURCE[0]}"
while [ -h "$src" ]; do # resolve $src until the file is no longer a symlink
    dir="$( cd -P "$( dirname "$src" )" && pwd )"
    src="$(readlink "$src")"
    [[ $src != /* ]] && src="$dir/$src" # if $src was a relative symlink, we need to resolve it relative to the path where the symlink file was located
done
dir="$( cd -P "$( dirname "$src" )" && pwd )"

cd $dir

backup_dir=$(readlink -f "../backup/$(basename $dir)")
mkdir -p $backup_dir

echo "Making symbolic links for $(basename $dir)."

for f in "$dir"/*
do
    dotfile=~/.${f##*/}
    backup=$backup_dir/.${f##*/}
    t1=${f##*/}
    t2=${this_script##*/}
    # Skip this install script and any dotfiles in this directory.
    if [ "$t1" = "$t2" ] ||
       [ "$t1" = ".*" ] ; then
        continue
    fi
    # Check if the dotfile already exists
    if [ \( -f $dotfile -o -d $dotfile \) ]; then
        # If the dotfile is not a symbolic link,
        if [ ! -L $dotfile ]; then
            # Backup the file in ~/dotfiles_old/
            echo "'$dotfile' exists; backing up in '$backup'"
            mv $dotfile $backup
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
