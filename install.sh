#!/bin/bash
set -e

# Check if git is installed.
git --version >/dev/null 2>&1 || {
    echo >&2 "Depends on git, aborting."
    exit 1
}

this_script=$0
this_script_args=$@
dotfiles=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )
dotfiles_old=~/dotfiles_old

mkdir -p $dotfiles_old
cd $dotfiles


help_msg() {
    echo "usage: $this_script [-hcu]"
    echo "  -h --help             Display this message."
    echo "  -c --clone-plugins    Clone vim plugins."
    echo "  -u --update           Updates, then reruns the script minus the"
    echo "                        '-u' and '--update' flags."
    exit 0
}

clone_plugins() {
    # Clone the vim plugins.
    echo "Cloning vim plugins, you may need to build some of them manually."
    git submodule update --init --recursive
    echo "Done cloning plugins."
}

update() {
    _new_args=""
    for arg in $this_script_args
    do
        if [ "$arg" == "-u" ] || [ "$arg" == "--update" ] ; then
            continue
        else
            _new_args="$_new_args $arg"
        fi
    done
    echo "Updating..."
    git pull
    $this_script $_new_args
    echo "Done updating."
    exit 0
}

make_sym_links() {
    echo "Making symbolic links for dotfiles."
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
    echo "Done making symbolic links."
}

for arg in $this_script_args
do
    if [ "$arg" == "-h" ] || [ "$arg" == "--help" ] ; then
        help_msg_flag=true
    elif [ "$arg" == "-c" ] || [ "$arg" == "--clone-plugins" ] ; then
        clone_plugins_flag=true
    elif [ "$arg" == "-u" ] || [ "$arg" == "--update" ] ; then
        update_flag=true
    else
        echo "Uknown option '$arg'"
        unknown_options_flag=true
    fi
done

if [[ "$unknown_options_flag" ]] ; then
    echo "Stopping."
    exit 1
fi

if [[ "$update_flag" ]] ; then
    update
fi

if [[ "$help_msg_flag" ]] ; then
    help_msg
fi

if [[ "$clone_plugins_flag" ]] ; then
    clone_plugins
fi

make_sym_links

echo "Done."


