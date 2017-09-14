#!/bin/bash

wd=eval pwd                 # Save working dir to return after navigation.
version='0.1'               # Version of the program.
bakdir=$HOME/.env_backup    # Directory to store config backups.
vimdir=$HOME/.vim_runtime   # Directory containing Vim extras.

if [ $OSTYPE == 'linux-gnu' ]; then
    echo Detected OS is Linux

    if [ -d "$bakdir" ]; then
        read -p "Backup folder detected, continue? (y/n) " answer
        if [ ! "$answer" == 'y' ]; then
            exit
        fi
    else
        mkdir "$bakdir"
    fi

    echo "$version" > "$bakdir/.version"

    echo "Backing up existing config files to $bakdir"
	cp "$HOME/.bash_aliases" "$bakdir"
	cp "$HOME/.nanorc" "$bakdir"
    cp "$HOME/.bashrc" "$bakdir"

    echo Placing custom config files.
	cp ./.nanorc "$HOME"
	cp ./.bash_aliases "$HOME"

    echo Enabling aliases
    . "$HOME/.bash_aliases"

    if [ -d "$vimdir" ]; then
        echo Detected existing Vim runtime... checking update.
        cd "$vimdir"
        git rebase
        cd "$wd"
    else
        echo Installing Amix\'s Awesome Vim config
	    git clone --depth=1 https://github.com/amix/vimrc.git "$vimdir"
    fi
    sh "$vimdir/install_awesome_vimrc.sh"

elif [ $OSTYPE == 'darwin' ]; then
    echo You are running Mac
elif [ $OSTYPE == 'msys' ]; then
        echo You are using Git Bash on Windows
else
    echo "OS not set... Exiting with no change."
fi
