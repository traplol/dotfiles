#!/bin/bash
set -e

# Check if git is installed.
git --version >/dev/null 2>&1 || {
    echo >&2 "Depends on git, aborting."
    exit 1
}

this_script=$0
this_script_args=$(getopt -o hcung: -l help,clone-submodules,update,no-link,generate: -n "$(basename $this_script)" -- "$@")

source="${BASH_SOURCE[0]}"
while [ -h "$source" ]; do # resolve $source until the file is no longer a symlink
    dir="$( cd -P "$( dirname "$source" )" && pwd )"
    source="$(readlink "$source")"
    [[ $source != /* ]] && source="$dir/$source" # if $source was a relative symlink, we need to resolve it relative to the path where the symlink file was located
done
dir="$( cd -P "$( dirname "$source" )" && pwd )"
cd $dir
mkdir -p "backup"

help_msg() {
    echo "usage: $this_script [OPTIONS]"
    echo "  -h --help               Display this message."
    echo "  -c --clone-submodules   Clone git submodules."
    echo "  -u --update             Updates, then reruns the script minus the"
    echo "                          '-u' and '--update' flags."
    echo "  -n --no-link            Does not create symbolic links to dotfiles."
    echo "  -g --generate [NAME]    Creates a directory and copies bootstrap.sh.template"
    echo "                          to the new directory as bootstrap.sh."
    exit 0
}

clone_submodules() {
    # Clone submodules
    echo "Cloning submodules..."
    git submodule update --init --recursive
    echo "Done cloning submodules."
}

update() {
    # Check if there are any local changes to dotfiles or this install script.
    if git diff-index --quiet HEAD --; then
        echo "No local changes."
    else
        echo "Local changes found, run 'git diff HEAD' to see pending changes and consider stashing or pushing. "
        exit 1
    fi

    local _new_args=""
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

generate() {
    local gen_dir=$1
    if [[ "$gen_dir" == */* ]] ; then
        echo "This script will not parse more than one level deep, aborting."
        exit 1
    elif [ -e "$gen_dir" ] ; then
        echo "'$gen_dir' exists already, aborting. "
        exit 1
    else
        echo "Making bootstrap directory '$gen_dir'."
        mkdir "$gen_dir"
        cd $gen_dir
        cp "../bootstrap.sh.template" "./bootstrap.sh"
    fi
    exit 0
}

make_sym_links() {
    # Go into each directory and source the bootstrap
    for f in "$dir"/*
    do
        if [ -d "$f" ] &&               # directories
           [ "$f" != ".*" ] &&          # no .dotfiles
           [ "$f" != "backup" ] ; then  # skip backup directory
            if [ -f "$f/bootstrap.sh" ] ; then
                /bin/bash "$f/bootstrap.sh"
            fi
        fi
    done
}

while [ $# -gt 0 ]
do
    case "$1" in
        -h | --help ) help_msg_flag=true; shift ;;
        -c | --clone-submodules ) clone_submodules_flag=true; shift ;;
        -u | --update ) update_flag=true; shift ;;
        -n | --no-link ) no_symlink_flag=true; shift ;;
        -g | --generate ) generate_flag=true; generate_dir="$2"; shift; shift ;;
        -- ) shift; break ;;
        *  )
            echo "Uknown option '$1'"
            unknown_options_flag=true
            shift ;;
    esac
done


if [[ "$update_flag" ]] ; then
    update
fi

# Allow the script to update so that any flags used in the updated install
# script will be passed to it. This allows for the updated script to catch
# any unknown arguments or to process arguments that this script may not
# know.
if [[ "$unknown_options_flag" ]] ; then
    echo "Stopping."
    exit 1
fi

if [[ "$help_msg_flag" ]] ; then
    help_msg
fi

if [[ "$generate_flag" ]] ; then
    generate "$generate_dir"
fi

if [[ "$clone_submodules_flag" ]] ; then
    clone_submodules
fi

if [[ "$no_symlink_flag" ]] ; then
    : # do nothing!
else
    make_sym_links
fi

echo "Done."


