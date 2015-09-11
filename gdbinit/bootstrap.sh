#!/bin/bash
set -e

PREFIX=${HOME}

this_script=$0
this_script_args=$(getopt -o h -l help,remove-symbolic-links,create-symbolic-links -n "$(basename $this_script)" -- "$@")

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

help_msg() {
    echo "usage: $this_script [OPTION]"
    echo "  Creates or removes self-generated symbolic links in the"
    echo "  '$PREFIX' directory."
    echo ""
    echo "  -h --help                       Display this message."
    echo "  --create-symbolic-links         Creates symbolic links."
    echo "  --remove-symbolic-links         Removes symbolic links."
    echo ""
    exit 0
}

create_sym_links() {
    echo "Making symbolic links for $(basename $dir)."

    local dotfile=""
    local backup=""
    local t1=""
    local t2=""

    for f in "$dir"/*
    do
        dotfile=$PREFIX/.${f##*/}
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
}

remove_sym_links() {
    echo "Removing generated symbolic links for $(basename $dir)."

    local dotfile=""
    local backup=""
    local t1=""
    local t2=""

    for f in "$dir"/*
    do
        dotfile=$PREFIX/.${f##*/}
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
            # If the dotfile is a symbolic link,
            if [ -L $dotfile ]; then
                # Check if the dotfile/sym link exists and remove it
                if [ -e $dotfile ]; then
                    echo "Removing symbolic link: '$dotfile'."
                    rm $dotfile
                fi
                # Check if a backup exists.
                if [ -e $backup ]; then
                    mv $backup $dotfile
                    echo "Successfully moved '$backup' to '$dotfile'"
                fi
            else
                echo "'$dotfile' is not a symlink, skipping."
            fi
        fi
    done
}

while [ $# -gt 0 ]
do
    case "$1" in
        -h | --help ) help_msg_flag=true; shift ;;
        --create-symbolic-links ) create_symbolic_links_flag=true; shift ;;
        --remove-symbolic-links ) remove_symbolic_links_flag=true; shift ;;
        -- ) shift; break ;;
        *  )
            echo "Unknown option '$1'"
            unknown_options_flag=true
            shift ;;
    esac
done


if [[ "$unknown_options_flag" ]] ; then
    help_msg
fi

if [[ "$help_msg_flag" ]] ; then
    help_msg
fi

if [[ "$create_symbolic_links_flag" && "$remove_symbolic_links_flag" ]] ; then
    echo "Cannot both create and remove symbolic links!."
    exit 1
fi

if [[ "$create_symbolic_links_flag" ]] ; then
    create_sym_links
    exit 0
fi

if [[ "$remove_symbolic_links_flag" ]] ; then
    remove_sym_links
    exit 0
fi

# should exit before we get here if any arguments were passed.
# otherwise, if no arguments are passed show the help message.
help_msg
