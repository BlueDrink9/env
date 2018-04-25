#!/bin/bash

# For debugging use
# set -eEuxo pipefail
# set -uxo pipefail
WD="$PWD"                   # Save working dir to return after navigation.
SCRIPTDIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
BAKDIR=$HOME/.env_backup    # Directory to store config backups.
BASH_CUSTOM=$HOME/.bash_custom # Directory to store custom bash includes.
VIMDIR=$HOME/.vim_runtime   # Directory containing Vim extras.
# SCRIPT COLORS are kept in this file
source $SCRIPTDIR/bash/colour_variables.sh
OK="[ ${Green}OK${NC} ]"
Error="[ ${Red}ERROR${NC} ]"
TAB="\e[1A\e[2L"
SKIP=0
ALL=0
if [[ $OSTYPE == 'linux-gnu' ]]; then
    FONTDIR=$HOME/.fonts
elif [[ $OSTYPE =~ 'darwin' ]]; then
    FONTDIR=$HOME/Library/Fonts
fi
VSCODE_EXTENSIONS_DIR="${HOME}/.vscode/extensions"
VSCODE_VERSION=code
VSCODE_APP_DATA="${HOME}/AppData/Roaming/Code"

printLine() {
   printf -- "$@\n" 
}
printErr() {
   >&2 printLine "$@"
}

askQuestionYN() {
    default="?"
    question=${1:-default}
    echo -ne "${question} (y/n) " >&2
    read -n 1 REPLY
    printErr ""
    if [[ $REPLY =~ ^[yY]$ ]]; then
        return 0
    else
        return 1
    fi
}

getWebItem() {
    default="no-url-given"
    url=${1:-default}
    printErr "Downloading $url ... "
    # OSX has curl by default, linux has wget by default
    # if [[ $OSTYPE =~ 'darwin' ]]; then
    if hash curl 2> /dev/null; then
        curl -fL $url 2> /dev/null
    else
        wget -qO- $url 2> /dev/null
    fi
    printErr "Done"
}

downloadURLtoFile() {
    default="invalid url or filename"
    url=${1:-default}
    filename=${2:-default}
    if [ "$url" = "default" ] || [ "$filename" = "default" ]; then
        printErr "Error: Invalid url or filename"
    fi
    downloadDirectory=$(dirname "$filename")
    if [ ! -d "$downloadDirectory" ]; then
        mkdir -p "$downloadDirectory"
    fi
    getWebItem $url >| $filename
}

getLatestReleaseFileURL() {
    # Takes argument 1 of form user/repo, eg will-shaw/env.
    # Gets the URL of the latest-released version of the specified filename arg 2.
    default="invalid url or filename"
    repo=${1:-default}
    file=${2:-default}
    repo=$1
    file=$2
    repoapi=`getWebItem "https://api.github.com/repos/${repo}/releases/latest"`
    searchTemplate=https://github.com/${repo}/releases/download/[^/]*/${file}
    fileLatestURL=`echo $repoapi | sed -n -e "s,^.*\(${searchTemplate}\).*$,\1,p"`
    echo $fileLatestURL
}

downloadURLAndExtractZipTo() {
    # Two arguments: url, and destination folder.
    default="invalid url or filename"
    url=${1:-default}
    destDir=${2:-default}
    if [ "$url" = "default" ] || [ "$destDir" = "default" ]; then
       printErr "Error: Invalid url or dest"
    fi
    if [ ! -d "$destDir" ]; then
        mkdir -p $destDir
    fi
    tmpzipfile=$(mktemp)
    downloadURLtoFile $url $tmpzipfile.zip
    unzip -o $tmpzipfile.zip -d "$destDir" # > /dev/null
    rm -f $tmpzipfile.zip
}

addTextIfAbsent() {
    default="invalid text or filename"
    text=${1:-default}
    file=${2:-default}
    # Check if text exists in file, otherwise append.
    grep -q -F "$text" "$file" || echo "$text" >> "$file"
}

# -------------
# Currently only removes vim configs and any bash customisation, as well as this script.
uninstall() {
    if askQuestionYN "Really uninstall?"; then
        printf "\nUninstalling...\n"

        # The sed commands replace source lines with blanks
        rm -rf "${BASH_CUSTOM}"
        rm -rf "${HOME}/.vim_runtime"
        sed -in "s|.*vim_runtime.*||g" ${HOME}/.vimrc

        sed -in "s|.*[^.]bashrc.*||g" ${HOME}/.bashrc
        sed -in "s|.*${SCRIPTDIR}/terminal/tmux\.conf.*||g" ${HOME}/.tmux.conf
        rm -rf "${HOME}/.dircolours_solarized"
        sed -in "s|.*${SCRIPTDIR}/editors/vim/vimrc.*||g" ${HOME}/.vimrc
        sed -in "s|.*${SCRIPTDIR}/bash/inputrc.sh.*||g" ${HOME}/.inputrc
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

vscodeExtensions() {
    if [[ $ALL == 1 ]]; then
        REPLY="y"
    else
        askQuestionYN "${Yellow}Install Visual Studio Code extensions?$NC"
    fi
    if [[ $REPLY =~ ^[nN]$ ]]; then
        return 0
    elif [[ $REPLY =~ ^[yY]$ ]]; then # Install extensions from 'vscode/extensions'
        
        if hash code-insiders 2> /dev/null; then # Maybe insider version is being used.
            VSCODE_VERSION=code-insiders
            VSCODE_APP_DATA="${HOME}/AppData/Roaming/Code - Insiders/"
            VSCODE_EXTENSIONS_DIR="${HOME}/.vscode-insiders/extensions"
        fi
        if hash code 2> /dev/null || hash code-insiders 2> /dev/null; then # Check if 'code' exists.

            if [[ ! -d "$VSCODE_EXTENSIONS_DIR" ]]; then
                mkdir -p "$VSCODE_EXTENSIONS_DIR"
            fi

            while IFS='' read -r LINE || [[ -n "$LINE" ]]; do
                code --install-extension $LINE
            done < "${SCRIPTDIR}/editors/.vscode/extensions"

            # elif [[ $REPLY =~ ^[cC]$ ]]; then # Load VSCode which detects recommendations.json
            #     #TODO Where is this meant to be CDed to?
            #     code ./editors
        else
            printErr "VSCode not in PATH"
            return 1
        fi
    if hash code 2> /dev/null; then # Check if 'code' exists.
        VSCODE_EXTENSIONS_DIR="${HOME}/.vscode/extensions"
   
    else
        echo -e "VSCode not installed or variable not set."
        return 1
    fi
    if [[ $REPLY =~ ^[yY]$ ]]; then # Install extensions from 'vscode/extensions'
        if [[ ! -d "$VSCODE_EXTENSIONS_DIR" ]]; then
            mkdir -p "$VSCODE_EXTENSIONS_DIR"
        fi
        if [[ ! -d "${VSCODE_APP_DATA}/User" ]]; then
            mkdir -p "${VSCODE_APP_DATA}/User"
        fi

        while IFS='' read -r LINE || [[ -n "$LINE" ]]; do
            ${VSCODE_VERSION} --install-extension $LINE
        done < "${SCRIPTDIR}/editors/.vscode/extensions"

        cp "${SCRIPTDIR}/editors/settings.json" "${VSCODE_APP_DATA}/User"

    elif [[ $REPLY =~ ^[cC]$ ]]; then # Load VSCode which detects recommendations.json
        $VSCODE_VERSION ./editors
    fi
    return 0
}


installFonts() {
    if [  $ALL == 1 ] || askQuestionYN "${Yellow}Install fonts? $NC"; then
        mkdir -p "$FONTDIR"
        if [[ ! -d "${FONTDIR}/truetype/custom" ]]; then
            mkdir -p "${FONTDIR}/truetype/custom"
        fi

        printErr "Downloading fonts..."

        # Get latest Iosevka font release.
        fontUrl=`getLatestReleaseFileURL "be5invis/Iosevka" "iosevka-pack-[^z]*zip"`
        fontdir="${FONTDIR}/Iosevka"
        downloadURLAndExtractZipTo $fontUrl $fontdir && \
            printErr "${OK} Fonts installed to ${Yellow}${fontdir}${NC}" || \
            printErr "${Error} ${Red}Fonts failed to install to ${Yellow}${fontdir}${NC}"

        SCPUrl=`getLatestReleaseFileURL "ryanoasis/nerd-fonts" "SourceCodePro\.zip"`
        SCPdir="${FONTDIR}/SauceCodeProNF"
        downloadURLAndExtractZipTo $SCPUrl $SCPdir && \
            printErr "${OK} Fonts installed to ${Yellow}${SCPdir}${NC}" || \
            printErr "${Error} ${Red}Fonts failed to install to ${Yellow}${SCPdir}${NC}"

        if [[ $OSTYPE == 'linux-gnu' ]]; then
            # Unused mac-required SCP fonts.
            rm -f "${SCPdir}/*Windows Compatible.ttf"
        elif [[ $OSTYPE =~ 'darwin' ]]; then
            rm -f "${SCPdir}/*Complete.ttf"
            rm -f "${SCPdir}/*Mono.ttf"
        fi

        fc-cache && printErr "${OK} Fontcache updated" || \
            printErr "${Error} ${Red}Failed to update fontcache${NC}"


    fi
}

setVimColorscheme() {
    if [ ! -d "$HOME/.vim/colors" ] || [ ! $SKIP == 2 ]; then
        printErr "Downloading Vim colorschemes."
        git clone --depth=1 https://github.com/flazz/vim-colorschemes.git 2> /dev/null
        if [[ ! -d "$HOME/.vim" ]]; then
            mkdir -p "$HOME/.vim/colors"
        fi
        printErr "Placing color schemes..."
        cp ./vim-colorschemes/colors/*.vim "$HOME/.vim/colors" && \
            printErr "$OK Placing color schemes... Done." || \
            printErr "${Error}${Red}Failed previous command${NC}"

        rm -rf "./vim-colorschemes"
    fi

    echo -ne "${Yellow}Enter chosen color scheme name: $NC"
    read COLORSCHEME

    printErr "$OK Color scheme = $COLORSCHEME"

    sed 's/${VIM_COLORSCHEME}/'$COLORSCHEME'/g' ./editors/vim/extended.vim > ./extended.vim

    setVimLineNumbers
}

setVimLineNumbers() {
    if [  $ALL == 1 ] || askQuestionYN "${Yellow}Do you want to enable line numbers?${NC}"; then
        sed -i 's/${NUMBER}/ /g' ./extended.vim && \
            printErr "$OK Vim line numbers disabled." || \
            printErr "${Error}${Red}Failed previous command${NC}"

    else
        sed -i 's/${NUMBER}/set number/g' ./extended.vim && \
            printErr "$OK Vim line numbers enabled." || \
            printErr "${Error}${Red}Failed previous command${NC}"

    fi
    cp ./extended.vim $VIMDIR/vimrcs/extended.vim
    rm ./extended.vim
}

dualBootLocalTime() {
    if askQuestionYN "${Yellow}Interpret hardware clock as local time? ${NC}"; then
        timedatectl set-local-rtc 1 && \
            printErr "\r$OK Linux using local time." || \
            printErr "${Error}${Red}Failed previous command${NC}"

    else
        printErr ''
    fi
}

gitUser() {
    if [[ ${1:-} =~ ^--?[gG](it)?-?[cC](redentials)? ]]; then
        printErr "Forcing git update."
    elif [[ "$(git config --global user.name)" =~ .+ ]] && [[ $(git config --global user.email) =~ .+ ]]; then
        printErr "$OK Git global user is set."
        printErr "Re-run with ( ./install -gu ) to force update."
        return 0
    fi

    printErr "Setting up git global user..."
    echo -ne "${Green}Enter your github username:${NC} "
    read GIT_USER
    echo -ne "${Green}Enter your git email:${NC} "
    read GIT_EMAIL

    printErr "${Yellow}Configuring git.$NC"
    printErr "Username: $GIT_USER"
    printErr "Email: $GIT_EMAIL"
    git config --global user.name "$GIT_USER"
    git config --global credential.https://github.com.username "$GIT_USER"
    git config --global user.email "$GIT_EMAIL"
    printErr "${Cyan}You can update your git user by entering:$NC ./install -gu"
}

gitCredentialCache() {
    if [ $ALL == 1 ] || askQuestionYN "${Yellow}Want Git to store your credentials for a while?${NC}"; then

        if [[ ! "$(git config --global user.name)" =~ .+ ]] || [[ ! $(git config --global user.email) =~ .+ ]]; then
            printErr "$OK Git global user is not setup correctly."
            printErr "Re-run with ( ./install -gu ) to force update."
            return 0
        fi

        printErr "${Green}How long should Git store your credentials? (minutes)${NC} "
        while [ 1 ]; do
            read REPLY
            if [[ $REPLY =~ ^[0-9]+$ ]] && [[ $REPLY > 0 ]]; then
                break
            else
                printErr "${Red}Invalid input${NC}"
            fi
        done
        printErr "Git will remember your credentials for $REPLY minutes ($(( $REPLY * 60 )) seconds)."
        git config --global credential.helper "cache --timeout=$(( $REPLY * 60 ))"
    fi
}

gitSettings() {
    # If files are missing, git will silently ignore. So potentially ok to not
    # uninstall (would be messy/potentially damage other file).
    # This is only supported on git versions > 1.7.10.
    addTextIfAbsent "[include]
        path = ${SCRIPTDIR}/gitglobal/gitconfig" ${HOME}/.gitconfig
    addTextIfAbsent "[include]
        path = ${SCRIPTDIR}/gitglobal/gitignore" ${HOME}/.gitignore
    addTextIfAbsent "[include]
        path = ${SCRIPTDIR}/gitglobal/gitattributes" ${HOME}/.gitattributes
}

setupShell() {
    if [ "$IS_SHAW" == 0 ] ; then
        printErr "Copying custom bash files..."
        if [[ -d "${BASH_CUSTOM}" ]] && [[ ! $SKIP == 1 ]]; then
            printErr "${OK} ${BASH_CUSTOM} directory exists."
        else
            mkdir -p ${BASH_CUSTOM}
            cp -r $SCRIPTDIR/bash/* ${BASH_CUSTOM}/
            addTextIfAbsent "source $BASH_CUSTOM/bashrc" ${HOME}/.bashrc
        fi
    else
        printErr "Enabling custom bash setup..."
        addTextIfAbsent "source $SCRIPTDIR/bash/bashrc" ${HOME}/.bashrc
        downloadURLtoFile  \
            https://raw.githubusercontent.com/seebi/dircolors-solarized/master/dircolors.256dark  \
            "${HOME}/.dircolours_solarized"
            # https://raw.githubusercontent.com/seebi/dircolors-solarized/master/dircolors.ansi-universal \

        printErr "Enabling custom tmux setup..."
        addTextIfAbsent "source-file $SCRIPTDIR/terminal/tmux.conf" ${HOME}/.tmux.conf
        printErr "Enabling custom readline (inputrc) setup..."
        addTextIfAbsent "\$include $SCRIPTDIR/bash/inputrc.sh" ${HOME}/.inputrc
    fi
    if [[ $OSTYPE =~ 'darwin' ]]; then
        addTextIfAbsent "source .bashrc" ${HOME}/.bash_profile
    fi
}

setupVim(){
    if [ "$IS_SHAW" == 0 ] ; then

        printErr "Checking Vim..."

        if [[ -d "${VIMDIR}" ]]; then
            printErr "found custom Vim.\nUpdating => "

            cd "${VIMDIR}"
            git stash | xargs echo > /dev/null
            git rebase origin master | xargs echo -n
            git stash pop | xargs echo > /dev/null
            cd "${WD}"
            printErr "${OK} Vim configuration is up to date."
        else
            printErr "Installing Amix's Awesome Vim config"
            git clone --depth=1 https://github.com/amix/vimrc.git "$VIMDIR" && \
                printErr "${OK} Installed Amix's Awesome Vim config." || \
                printErr "${Error}${Red}Failed previous command${NC}"

        fi
        sh "${VIMDIR}/install_awesome_vimrc.sh" | xargs echo > /dev/null

        printErr ""
        printErr "------------------- VIM COLOR SCHEME"
        setVimColorscheme

    else
        printErr "Using WW's vimrc"
        addTextIfAbsent "so $SCRIPTDIR/editors/vim/vimrc" ${HOME}/.vimrc
        printErr "Installing vim plugins..."
        # Install Plug (plugin manager)
        downloadURLtoFile https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim "${HOME}/.vim/autoload/plug.vim"
        # This has the problem of making the caret disappear in WSL...
        vim -E +PlugInstall +qall
        # Recover missing cursor due to previous command
        reset
    fi
}

main() {
    echo -ne "Are you WS or WW? "
    while [ 1 ] ; do
        read -n 2 U
        printErr ""
        # Convert to upper case
        U=`echo "$U" | tr '[:lower:]' '[:upper:]'`
        if [ $U == "WS" ] || [ $U == "WW" ] ; then
            break
        else
            printErr "Who now? Are you sure?"
        fi
    done
    if [ $U == "WS" ] ; then
        IS_SHAW=0
    else
        IS_SHAW=1
    fi

    printErr ""
    printErr '------------------- VSCODE EXTENSIONS'
    vscodeExtensions

    printErr ""
    printErr "------------------- GIT"
    gitUser ${1:-}
    gitCredentialCache

    if [ "$IS_SHAW" == 0 ] ; then
        printErr ""
        printErr "------------------- LOCAL TIME"
        dualBootLocalTime
    fi

    printErr ""
    printErr "------------------- SHELL"
    setupShell

    printErr ""
    printErr "------------------- FONTS"
    installFonts

    printErr ""
    printErr "------------------- VIM"
    # setupVim

    printErr "${Green} Install Complete${NC}"
    # Restart bash
    exec bash

    return 0
}

# set default arg to avoid warnings
arg1=${1:-}
if [ ! -z $arg1 ]; then
    if [[ $arg1 = "-u" ]]; then
        uninstall
        exit
    fi

    if [[ $arg1 =~ ^--?[aA][lL]{2}?$ ]]; then
        ALL=1
    fi
fi

if [[ $OSTYPE == 'linux-gnu' ]]; then

    printErr "[$Green Linux ${NC}]"
    main ${1:-}

elif [[ $OSTYPE =~ 'darwin' ]]; then
    printErr "${Red}MacOS not fully supported."
    if askQuestionYN "Continue anyway?" ; then
        main $1
    fi
elif [[ $OSTYPE == 'msys' ]]; then
    printErr "${Red}Git Bash not supported."
    if askQuestionYN "Continue anyway?" ; then
        printLine "${Red}Attempting install on Git Bash."
        main $1
    fi
else
    printErr "OS not set... Exiting without change."
fi
