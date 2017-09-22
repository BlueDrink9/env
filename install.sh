#!/bin/bash

WD="$PWD"                   # Save working dir to return after navigation.
VERSION='1.1'               # Version of the program.
BAKDIR=$HOME/.env_backup    # Directory to store config backups.
VIMDIR=$HOME/.vim_runtime   # Directory containing Vim extras.
FONTDIR="/usr/local/share/fonts"
SKIP=0
GIT_UPDATE=0

echo -ne Detecting OS...

function copyFonts() {
    unzip -o "./fonts/*.zip" -d "./fonts" -x "woff/*" "woff2/*"
    sudo cp ./fonts/ttf/* $FONTDIR/truetype/custom
    sudo chown root $FONTDIR/truetype/custom/*.ttf
    fc-cache
    rm -rf "./fonts/ttf"
}

if [[ $OSTYPE == 'linux-gnu' ]]; then

    echo -e "[\e[32mLinux\e[0m]"

    if [[ $1 =~ -[Yy]$ ]]; then
        SKIP=1
    fi

    if [[ $1 =~ -[Gg][Uu]$ ]]; then
        GIT_UPDATE=1
    fi

    echo -ne "Creating backups..."
    if [[ -d "$BAKDIR" ]] && [[ ! $SKIP == 1 ]]; then
        echo -ne "[\e[31mExisting backup detected\e[0m]\n\e[33mContinue? (y/n) \e[0m"
        read -n 1 REPLY
        echo ""
        if [[ ! $REPLY =~ ^[Yy]$ ]]; then
            exit
        fi
    else
        mkdir -p "$BAKDIR"
    fi

    if [[ -f "$BAKDIR/.version" ]]; then
        echo -ne "Checking existing version..."
        EXISTING=$(< "$BAKDIR/.version")
        if [[ "$EXISTING" == "$VERSION" ]]; then
            echo -e "Detected [\e[32m$VERSION\e[0m] Skipping to check Vim."
            SKIP=2
        else
            sudo echo "This gets sudo" > /dev/null
            out="cat $BAKDIR/.version"
            echo $out
            echo -e "Detected [\e[31m$EXISTING\e[0m] Updating => [\e[32m$VERSION\e[0m]"
        fi
    fi

    if [[ ! "$SKIP" == 2 ]] || [[ "$GIT_UPDATE" == 1 ]]; then
        echo -e "\e[33mSetting up git global user...\e[0m"
        echo -ne "\e[34mEnter your git username:\e[0m "
        read GIT_USER
        echo -ne "\e[34mEnter your git email:\e[0m "
        read GIT_EMAIL

        echo -e "\e[32mConfiguring git.\e[0m"
        echo "Username: $GIT_USER"
        echo "Email: $GIT_EMAIL"
        git config --global user.name "$GIT_USER"
        git config --global user.email "$GIT_EMAIL"
        echo -e "\e[36mYou can update your git user by entering:\e[0m ./install -gu"
        echo ""
    fi

    if [[ ! "$SKIP" == 2 ]]; then
        echo "Updating installed version."
        echo "$VERSION" > "$BAKDIR/.version"

        echo -ne "Backing up existing config files to \e[36mfile://$BAKDIR\e[0m..."
        cp "$HOME/.bash_aliases" "$BAKDIR"
        cp "$HOME/.nanorc" "$BAKDIR"
        cp "$HOME/.bashrc" "$BAKDIR"
        echo -e "[\e[32mOK\e[0m]"

        echo -n "Placing custom config files..."
        cp ./editors/nanorc.sh "$HOME/.nanorc"
        cp ./bash/bash_aliases.sh "$HOME/.bash_aliases"
        cp ./bash/bashrc.sh "$HOME/.bashrc"
        echo -e "[\e[32mOK\e[0m]"

        echo -e "Installing fonts... (requires root)"
        if [[ ! -d "$FONTDIR/truetype/custom" ]]; then
            sudo mkdir -p "$FONTDIR/truetype/custom"
            copyFonts
        else
            copyFonts
        fi

        echo -n "Enabling bashrc..."
        cd "$HOME"
        source .bashrc

        echo -e "[\e[32mOK\e[0m]"

        echo -n "Enabling aliases..."
        source .bash_aliases
        cd "$WD"
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
