#!/bin/bash

WD="$PWD"                   # Save working dir to return after navigation.
VERSION='1.6'               # Version of the program.
BAKDIR=$HOME/.env_backup    # Directory to store config backups.
VIMDIR=$HOME/.vim_runtime   # Directory containing Vim extras.
FONTDIR="/usr/local/share/fonts"
SKIP=0

echo -ne Detecting OS...

function copyFonts() {
    unzip -o "./fonts/*.zip" -d "./fonts" -x "woff/*" "woff2/*"
    sudo cp ./fonts/ttf/* $FONTDIR/truetype/custom
    sudo chown root $FONTDIR/truetype/custom/*.ttf
    fc-cache
    rm -rf "./fonts/ttf"
}

function setVimColorscheme() {
    if [ ! -d "$HOME/.vim/colors" ] || [ ! $SKIP == 2 ]; then
        echo -e "Installing Vim colorschemes."
        git clone --depth=1 https://github.com/flazz/vim-colorschemes.git
        if [[ ! -d "$HOME/.vim" ]]; then
            mkdir -p "$HOME/.vim/colors"
        fi
        echo -ne "Placing color schemes..."
        cp ./vim-colorschemes/colors/*.vim "$HOME/.vim/colors"
        echo -e "\r[\e[32m   OK   \e[0m] Placing color schemes...Done."
        rm -rf "./vim-colorschemes"
    fi

    echo -ne "\e[33mEnter chosen color scheme name: \e[0m"
    read COLORSCHEME

    echo -e "\e[1A\e[2L\e[32mYour colorscheme:\e[0m $COLORSCHEME"

    sed 's/${VIM_COLORSCHEME}/'$COLORSCHEME'/g' ./editors/extended.vim > "$VIMDIR/vimrcs/extended.vim"

    setVimLineNumbers
}

function setVimLineNumbers() {
    echo -ne "\e[33mDo you want to enable line numbers? (y/N)\e[0m "
    read -n 1 REPLY
    if [[ ! $REPLY =~ ^[yY]$ ]]; then
        echo -e "\r[\e[32m   OK   \e[0m] Vim line numbers disabled.       "
        sed -i 's/${NUMBER}/ /g' "$VIMDIR/vimrcs/extended.vim"
    else
        echo -e "\r[\e[32m   OK   \e[0m] Vim line numbers enabled.        "
        sed -i 's/${NUMBER}/set number/g' "$VIMDIR/vimrcs/extended.vim"
    fi
}

function updateGitUser() {
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
}

if [[ $OSTYPE == 'linux-gnu' ]]; then

    echo -e "[\e[32mLinux\e[0m]"

    if [[ $1 =~ -[Yy]$ ]]; then
        SKIP=1
    fi

    if [[ $1 =~ -[Gg][Uu]$ ]]; then
        updateGitUser
        exit
    fi

    if [[ $1 =~ -[vV][cC]$ ]]; then
        setVimColorscheme
        exit
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
            echo -e "\r[\e[32m   OK   \e[0m] Detected [\e[32m$VERSION\e[0m] Skipping to Vim."
            SKIP=2
        else
            sudo echo "This gets sudo, which is required for installing fonts." > /dev/null
            out="cat $BAKDIR/.version"
            echo $out
            echo -e "\r[\e[32m   OK   \e[0m] Updating version [\e[31m$EXISTING\e[0m] => [\e[32m$VERSION\e[0m]"
        fi
    fi

    if [[ ! "$SKIP" == 2 ]]; then

        updateGitUser

        echo "Updating installed version."
        echo "$VERSION" > "$BAKDIR/.version"

        echo -ne "Backing up existing config files to \e[36mfile://$BAKDIR\e[0m..."
        cp "$HOME/.bash_aliases" "$BAKDIR"
        cp "$HOME/.nanorc" "$BAKDIR"
        cp "$HOME/.bashrc" "$BAKDIR"
        echo -e "\r[\e[32m   OK   \e[0m] Existing configs can be found here \e[36mfile://$BAKDIR\e[0m"

        echo -n "Placing custom config files..."
        cp ./editors/nanorc.sh "$HOME/.nanorc"
        cp ./bash/bash_aliases.sh "$HOME/.bash_aliases"
        cp ./bash/bashrc.sh "$HOME/.bashrc"
        echo -e "\r[\e[32m   OK   \e[0m] Custom bash configs placed."

        echo -e "Installing fonts... (requires root)"
        if [[ ! -d "$FONTDIR/truetype/custom" ]]; then
            sudo mkdir -p "$FONTDIR/truetype/custom"
            copyFonts
        else
            copyFonts
        fi

        echo -n "Enabling bashrc..."
        cd "$HOME"
        source ".bashrc"

        echo -e "\r[\e[32m   OK   \e[0m] Enabled bashrc (requires terminal restart)"

        echo -n "Enabling aliases..."
        source ".bash_aliases"
        cd "$WD"
        echo -e "\r[\e[32m   OK   \e[0m] Enabled bash_aliases"
    fi

    echo -ne "Checking Vim..."

    if [[ -d "$VIMDIR" ]]; then
        echo -ne "found custom Vim.\nUpdating => "

        cd "$VIMDIR"
        git stash | xargs echo > /dev/null
        git rebase | xargs echo -n
        git stash pop | xargs echo > /dev/null
        cd "$WD"
        echo -e "\r\e[2K[\e[32m   OK   \e[0m] Vim configuration is up to date."
    else
        echo -ne Installing Amix\'s Awesome Vim config
	    git clone --depth=1 https://github.com/amix/vimrc.git "$VIMDIR"
        echo -e "\r[\e[32m   OK   \e[0m] Installed Amix'\s Awesome Vim config."
    fi
    sh "$VIMDIR/install_awesome_vimrc.sh" | xargs echo > /dev/null

    if [[ ! "$SKIP" == 2 ]]; then
        setVimColorscheme
    fi

    echo -e "[\e[32mInstall Complete\e[0m]"

elif [[ $OSTYPE == 'darwin' ]]; then
    echo You are running Mac
elif [[ $OSTYPE == 'msys' ]]; then
        echo You are using Git Bash on Windows
else
    echo "OS not set... Exiting with no change."
fi

