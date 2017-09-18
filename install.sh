#!/bin/bash

WD="$PWD"                   # Save working dir to return after navigation.
VERSION='0.4'               # Version of the program.
BAKDIR=$HOME/.env_backup    # Directory to store config backups.
VIMDIR=$HOME/.vim_runtime   # Directory containing Vim extras.
SKIP='0'

echo -ne Detecting OS...

if [[ $OSTYPE == 'linux-gnu' ]]; then
    echo -e "[\e[32mLinux\e[0m]"

    echo -ne "Creating backups..."
    if [[ -d "$BAKDIR" ]]; then
        echo -ne "existing backup detected.\n\e[33mContinue? (y/n) \e[0m"
        read -n 1 REPLY
        echo ""
        if [[ ! $REPLY =~ ^[Yy]$ ]]; then
            exit
        fi
    else
        mkdir "$BAKDIR"
    fi

    if [[ -f "$BAKDIR/.version" ]]; then
        echo -ne "Checking existing version..."
        EXISTING=$(< "$BAKDIR/.version")
        if [[ "$EXISTING" == "$VERSION" ]]; then
            echo -e "Detected [\e[32m$VERSION\e[0m] Skipping to check Vim."
            SKIP='2'
        else
            out="cat $BAKDIR/.version"
            echo $out
            echo -e "Detected [\e[31m$EXISTING\e[0m] Updating => [\e[32m$VERSION\e[0m]"
        fi
    fi

    if [[ ! "$SKIP" == '2' ]]; then
        echo "$VERSION" > "$BAKDIR/.version"

        echo -ne "Backing up existing config files to \e[36mfile://$BAKDIR\e[0m..."
        cp "$HOME/.bash_aliases" "$BAKDIR"
        cp "$HOME/.nanorc" "$BAKDIR"
        cp "$HOME/.bashrc" "$BAKDIR"
        echo -e "[\e[32mOK\e[0m]"

        echo -n "Placing custom config files..."
        cp ./editors/nanorc.sh "$HOME/.nanorc"
        cp ./bash/bash_aliases.sh "$HOME/.bash_aliases"
        echo -e "[\e[32mOK\e[0m]"

        echo -n "Enabling aliases..."
        . "$HOME/.bash_aliases"
        echo -e "[\e[32mOK\e[0m]"
    fi

    echo -ne "Checking Vim..."

    if [[ -d "$VIMDIR" ]]; then
        echo -ne "found custom Vim.\nUpdating => "

        cd "$VIMDIR"
        git rebase | xargs echo -n
        cd "$WD"
        echo -e " [\e[32mOK\e[0m]"
    else
        echo Installing Amix\'s Awesome Vim config
	    git clone --depth=1 https://github.com/amix/vimrc.git "$VIMDIR"
    fi
    sh "$VIMDIR/install_awesome_vimrc.sh" 2> /dev/null

    echo -e "[\e[32mInstall Complete\e[0m]"

elif [[ $OSTYPE == 'darwin' ]]; then
    echo You are running Mac
elif [[ $OSTYPE == 'msys' ]]; then
        echo You are using Git Bash on Windows
else
    echo "OS not set... Exiting with no change."
fi
