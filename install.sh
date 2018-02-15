#!/bin/bash

WD="$PWD"                   # Save working dir to return after navigation.
SCRIPTDIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
BAKDIR=$HOME/.env_backup    # Directory to store config backups.
BASH_CUSTOM=$HOME/.bash_custom # Directory to store custom bash includes.
VIMDIR=$HOME/.vim_runtime   # Directory containing Vim extras.
FONTDIR=$HOME/.fonts
SKIP=0

# SCRIPT COLORS are kept in file "colour_variables"
OK="[$Green  OK  ]$White"
TAB="\e[1A\e[2L"

# -------------

DONE=1
while [ ! $DONE ] ; do
    echo "Are you WS or WW?"
    read USER
    if [ $USER == "WS" ] || [ $USER == "WW" ] ; then
        DONE=0
    fi
done

if [ $USER == "WS" ] ; then
    IS_SHAW=0
else
    IS_SHAW=1
fi


echo -ne Detecting OS...

function copyFonts() {
    mkdir -p "$FONTDIR"
    unzip -o "./fonts/*.zip" -d "./fonts" -x "woff/*" "woff2/*" | xargs > /dev/null
    cp ./fonts/ttf/* $FONTDIR/truetype/custom
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
        echo -e "\r$OK Placing color schemes...Done."
        rm -rf "./vim-colorschemes"
    fi

    echo -ne "\e[33mEnter chosen color scheme name: \e[0m"
    read COLORSCHEME

    echo -e "\e[1A\e[2L$OK Color scheme = $COLORSCHEME"

    sed 's/${VIM_COLORSCHEME}/'$COLORSCHEME'/g' ./editors/extended.vim > "$VIMDIR/vimrcs/extended.vim"

    setVimLineNumbers
}

function setVimLineNumbers() {
    echo -ne "\e[33mDo you want to enable line numbers? (y/N)\e[0m "
    read -n 1 REPLY
    if [[ ! $REPLY =~ ^[yY]$ ]]; then
        echo -e "\r$OK Vim line numbers disabled.       "
        sed -i 's/${NUMBER}/ /g' "$VIMDIR/vimrcs/extended.vim"
    else
        echo -e "\r$OK Vim line numbers enabled.        "
        sed -i 's/${NUMBER}/set number/g' "$VIMDIR/vimrcs/extended.vim"
    fi
}

function dualBootLocalTime() {
    echo -ne "\e[33mInterpret hardware clock as local time? (y/N)\e[0m "
    read -n 1 REPLY
    if [[ $REPLY =~ ^[yY]$ ]]; then
        echo -e "\r$OK Linux using local time."
        timedatectl set-local-rtc 1
    else
        echo ''
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

    echo -e "[$GREEN Linux $WHITE]"

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

    dualBootLocalTime

    echo -e "Copying custom bash files..."
    if [[ -d "${BASH_CUSTOM}" ]] && [[ ! $SKIP == 1 ]]; then
        echo -e "${OK} ${BASH_CUSTOM} directory exists."
    else
        mkdir -p "${BAKDIR}"

        mkdir -p "${BASH_CUSTOM}"

        cp -r ./bash/* ${BASH_CUSTOM}/

        echo -n "Enabling custom bash setup..."
        if IS_SHAW ; then
            cp source $SCRIPTDIR/bash/bash_custom >> ${HOME}/.bash_custom
            echo "source ${HOME}/.bash_custom/bash_custom" >> ${HOME}/.bashrc
        else
            echo "source $SCRIPTDIR/bash/bash_custom" >> ${HOME}/.bashrc
        fi
    fi


    if [[ ! "$SKIP" == 2 ]]; then

        if [[ "$(git config --global user.name)" =~ .+ ]] && [[ $(git config --global user.email) =~ .+ ]]; then
            echo -e "$OK Git global user is set."
            echo -e "${TAB}Re-run with ( ./install -gu ) to force update."
        else
            updateGitUser
        fi

        echo "Updating installed version."
        echo "${VERSION}" > "${BAKDIR}/.version"

        echo -e "Installing fonts... "
        if [[ ! -d "${FONTDIR}/truetype/custom" ]]; then
            mkdir -p "${FONTDIR}/truetype/custom"
        fi
        copyFonts
        echo -e "${OK} Fonts installed to ${ORANGE}file:///${FONTDIR}${WHITE}"

        cd "$WD"
    fi

    echo -ne "Checking Vim..."

    if [[ -d "${VIMDIR}" ]]; then
        echo -ne "found custom Vim.\nUpdating => "

        cd "${VIMDIR}"
        git stash | xargs echo > /dev/null
        git rebase origin master | xargs echo -n
        git stash pop | xargs echo > /dev/null
        cd "${WD}"
        echo -e "\r\e[2K${OK} Vim configuration is up to date."
    else
        echo -ne Installing Amix\'s Awesome Vim config and WW\'s vimrc
        echo "so $SCRIPTDIR/editors/vimrc" >> ${HOME}/.vimrc
        git clone --depth=1 https://github.com/amix/vimrc.git "$VIMDIR"
        echo -e "\r[\e[32m   OK   \e[0m] Installed Amix'\s Awesome Vim config."
    fi
    sh "${VIMDIR}/install_awesome_vimrc.sh" | xargs echo > /dev/null

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

