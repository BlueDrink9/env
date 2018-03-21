#!/bin/bash

WD="$PWD"                   # Save working dir to return after navigation.
SCRIPTDIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
BAKDIR=$HOME/.env_backup    # Directory to store config backups.
BASH_CUSTOM=$HOME/.bash_custom # Directory to store custom bash includes.
VIMDIR=$HOME/.vim_runtime   # Directory containing Vim extras.
FONTDIR=$HOME/.fonts
SKIP=0

ALL=0

VSCODE_EXTENSIONS_DIR=$HOME/.vscode/extensions

function askQuestionYN() { 
    echo -ne $1 " (y/n)" >&2
    read -n 1 REPLY
    if [[ $REPLY =~ ^[yY]$ ]]; then
        return 0
    else
        return 1
    fi
}


# SCRIPT COLORS are kept in this file
# source $SCRIPTDIR/bash/colour_variables
OK="[ $Green  OK  ] $NC"
TAB="\e[1A\e[2L"


# -------------
# Currently only removes vim configs and any bash customisation, as well as this script.
function uninstall() {
    if askQuestionYN "Really uninstall? "; then
        printf "Uninstalling...\n"

        # The sed commands replace source lines with blanks
        rm -rf "${BASH_CUSTOM}"
        rm -rf "${HOME}/.vim_runtime"
        sed -in "s|.*vim_runtime.*||g" ${HOME}/.vimrc

        sed -in "s|.*[^.]bashrc.*||g" ${HOME}/.bashrc
        sed -in "s|.*${SCRIPTDIR}/terminal/tmux\.conf.*||g" ${HOME}/.tmux.conf
        sed -in "s|.*${SCRIPTDIR}/editors/vim/vimrc.*||g" ${HOME}/.vimrc
        rm -rf "${HOME}/vimfiles"
        rm -f "${HOME}/.vim/autoload/plug.vim"

        if askQuestionYN "Remove stored git username and email?"; then
            git config --global --unset-all user.name
            git config --global --unset-all credential.https://github.com.username
            git config --global --unset-all user.email
        fi

        # Reset bash
        exec bash
        # Remove self
        #TODO remove comment to exit debugmode. Check that any changes are pushed...
        # rm -rf "${SCRIPTDIR}"
    fi
}
if [[ $1 = "-u" ]]; then
    uninstall
    exit
fi


echo "Are you WS or WW?"
while [ 1 ] ; do
    read -n 2 U
    # Convert to upper case
    U=`echo "$U" | tr '[:lower:]' '[:upper:]'`
    if [ $U == "WS" ] || [ $U == "WW" ] ; then
        break
    else
        echo "Who now? Are you sure?"
    fi
done
if [ $U == "WS" ] ; then
    IS_SHAW=0
else
    IS_SHAW=1
fi

if [[ $1 =~ ^--?[aA][lL]{2}?$ ]]; then
    ALL=1
fi



function vscodeExtensions() {
    if [[ $ALL == 1 ]]; then
        REPLY="y"
    else
        echo -ne "\e[33mInstall Visual Studio Code extensions? (Y/n/c)\e[0m "
        read -n 1 REPLY
    fi
    if hash code 2> /dev/null; then # Check if 'code' exists.

        if [[ $REPLY =~ ^[yY]$ ]]; then # Install extensions from 'vscode/extensions'

            if [[ ! -d "$VSCODE_EXTENSIONS_DIR" ]]; then
                mkdir -p "$VSCODE_EXTENSIONS_DIR"
            fi

            while IFS='' read -r LINE || [[ -n "$LINE" ]]; do
                code --install-extension $LINE
            done < "${SCRIPTDIR}/editors/.vscode/extensions"

        elif [[ $REPLY =~ ^[cC]$ ]]; then # Load VSCode which detects recommendations.json
            #TODO Where is this meant to be CDed to?
            code ./editors
        fi
    else
        echo -e "VSCode not installed or variable not set."
        return 1
    fi
    return 0
}


function copyFonts() {
    if [  $ALL == 1 ] || askQuestionYN "\e[33mInstall fonts? \e[0m "; then
        if [[ ! -d "${FONTDIR}/truetype/custom" ]]; then
            mkdir -p "${FONTDIR}/truetype/custom"
        fi
        mkdir -p "$FONTDIR"
        cp ./fonts/* $FONTDIR/truetype/custom
        curl -fLo ~/.vim/autoload/plug.vim --create-dirs \
            https://github.com/ryanoasis/nerd-fonts/blob/1.2.0/patched-fonts/SourceCodePro/Medium/complete/Sauce%20Code%20Pro%20Medium%20Nerd%20Font%20Complete%20Mono.ttf --silent


        fc-cache
        echo -e "${OK} Fonts installed to ${Orange}file:///${FONTDIR}${White}"
    fi
}

function setVimColorscheme() {
    if [ ! -d "$HOME/.vim/colors" ] || [ ! $SKIP == 2 ]; then
        echo -e "Downloading Vim colorschemes."
        git clone --depth=1 https://github.com/flazz/vim-colorschemes.git 2> /dev/null
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

    sed 's/${VIM_COLORSCHEME}/'$COLORSCHEME'/g' ./editors/vim/extended.vim > ./extended.vim

    setVimLineNumbers
}

function setVimLineNumbers() {
    if [  $ALL == 1 ] || askQuestionYN "\e[33mDo you want to enable line numbers?\e[0m "; then
        echo -e "\n$OK Vim line numbers disabled."
        sed -i 's/${NUMBER}/ /g' ./extended.vim
    else
        echo -e "\n$OK Vim line numbers enabled."
        sed -i 's/${NUMBER}/set number/g' ./extended.vim
    fi
    cp ./extended.vim $VIMDIR/vimrcs/extended.vim
    rm ./extended.vim
}

function dualBootLocalTime() {
    if askQuestionYN "\e[33mInterpret hardware clock as local time? \e[0m "; then
        echo -e "\r$OK Linux using local time."
        timedatectl set-local-rtc 1
    else
        echo ''
    fi
}

function gitUser() {
    if [[ $1 =~ ^--?[gG](it)?-?[cC](redentials)? ]]; then
        echo -e "Force git update."
    elif [[ "$(git config --global user.name)" =~ .+ ]] && [[ $(git config --global user.email) =~ .+ ]]; then
        echo -e "$OK Git global user is set."
        echo -e "Re-run with ( ./install -gu ) to force update."
        return 0
    fi

    echo -e "Setting up git global user..."
    echo -ne "${Green}Enter your github username:${NC} "
    read GIT_USER
    echo -ne "${Green}Enter your git email:${NC} "
    read GIT_EMAIL

    echo -e "\e[32mConfiguring git.\e[0m"
    echo "Username: $GIT_USER"
    echo "Email: $GIT_EMAIL"
    git config --global user.name "$GIT_USER"
    git config --global credential.https://github.com.username "$GIT_USER"
    git config --global user.email "$GIT_EMAIL"
    echo -e "\e[36mYou can update your git user by entering:\e[0m ./install -gu"
}

function gitCredentialCache() {
    if [ $ALL == 1 ] || askQuestionYN "\e[33mWant Git to store your credentials for a while? \e[0m "; then

        if [[ ! "$(git config --global user.name)" =~ .+ ]] || [[ ! $(git config --global user.email) =~ .+ ]]; then
            echo -e "$OK Git global user is not setup correctly."
            echo -e "Re-run with ( ./install -gu ) to force update."
            return 0
        fi

        echo -ne "\n\e[33mHow long should Git store your credentials? (minutes)\e[0m "
        while [ 1 ]; do
            read REPLY
            if [[ $REPLY =~ ^[0-9]+$ ]] && [[ $REPLY > 0 ]]; then
                break
            else
                echo -e "${Red}Invalid input${NC}"
            fi
        done
        echo -e "Git will remember your credentials for $REPLY minutes ($(( $REPLY * 60 )) seconds)."
        git config --global credential.helper "cache --timeout=$(( $REPLY * 60 ))"
    fi
}

function setupShell() {
    if [ "$IS_SHAW" == 0 ] ; then
        echo -e "Copying custom bash files..."
        if [[ -d "${BASH_CUSTOM}" ]] && [[ ! $SKIP == 1 ]]; then
            echo -e "${OK} ${BASH_CUSTOM} directory exists."
        else
            mkdir -p ${BASH_CUSTOM}
            cp -r $SCRIPTDIR/bash/* ${BASH_CUSTOM}/
            echo "source $BASH_CUSTOM/bashrc" >> ${HOME}/.bashrc
        fi
    else
        echo -n "Enabling custom bash setup..."
        echo "source $SCRIPTDIR/bash/bashrc" >> ${HOME}/.bashrc
        echo -n "Enabling custom tmux setup..."
        echo "source-file $SCRIPTDIR/terminal/tmux.conf" >> ${HOME}/.tmux.conf
    fi
}

function setupVim(){
    if [ "$IS_SHAW" == 0 ] ; then

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
            echo -ne Installing Amix\'s Awesome Vim config
            git clone --depth=1 https://github.com/amix/vimrc.git "$VIMDIR"
            echo -e "\r[\e[32m   OK   \e[0m] Installed Amix'\s Awesome Vim config."
        fi
        sh "${VIMDIR}/install_awesome_vimrc.sh" | xargs echo > /dev/null

        echo -e "\n------------------- VIM COLOR SCHEME"
        setVimColorscheme

    else
        echo "Using WW's vimrc"
        echo "so $SCRIPTDIR/editors/vim/vimrc" >> ${HOME}/.vimrc
        echo "Installing vim plugins..."
        # Install Plug (plugin manager)
        curl -fLo ~/.vim/autoload/plug.vim --create-dirs \
            https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim --silent
        # This has the problem of making the caret disappear in WSL...
        vim -E +PlugInstall +qall
        # Recover missing cursor due to previous command
        reset
    fi
}

function main() {

    echo -e "\n------------------- VSCODE EXTENSIONS"
    vscodeExtensions

    echo -e "\n------------------- GIT"
    gitUser $1
    gitCredentialCache

    if [ "$IS_SHAW" == 0 ] ; then
        echo -e "\n------------------- LOCAL TIME"
        dualBootLocalTime
    fi

    echo -e "\n------------------- SHELL"
    setupShell

    echo -e "\n------------------- VIM"
    setupVim


    # Restart bash
    exec bash

    return 0
}

if [[ $OSTYPE == 'linux-gnu' ]]; then

    echo -e "[$Green Linux $White]"

    main $1

    echo -e "${Green} Install Complete${NC}"

elif [[ $OSTYPE =~ 'darwin' ]]; then
    echo -e "${Red}MacOS not fully supported."
    if askQuestionYN "Continue anyway?" ; then
        main $1
    fi
elif [[ $OSTYPE == 'msys' ]]; then
    echo -e "${Red}Git Bash not supported."
    if askQuestionYN "Continue anyway?" ; then
        main $1
    fi
else
    echo "OS not set... Exiting without change."
fi

