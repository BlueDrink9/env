#!/usr/bin/env bash

# {[} Setup and variables
# For debugging use
# set -eEuxo pipefail
# set -uxo pipefail
WD="$PWD"                   # Save working dir to return after navigation.
SCRIPTDIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
# SCRIPT COLORS are kept in this file
source "$SCRIPTDIR/bash/colour_variables.sh"
source "$SCRIPTDIR/bash/functions.sh"
source "$SCRIPTDIR/bash/script_functions.sh"
OK="[ ${Green}OK${NC} ]"
Error="[ ${Red}ERROR${NC} ]"
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

# {]} Setup and variables


# {[} Uninstall
uninstall() {
    if askQuestionYN "Really uninstall?"; then
        printf "\nUninstalling...\n"

        # The sed commands replace source lines with blanks
        rm -rf "${BASH_CUSTOM}"
        rm -rf "${HOME}/.config/nvim"
        rm -rf "${HOME}/.vim_runtime"
        sed -in "s|.*vim_runtime.*||g" ${HOME}/.vimrc

        sed -in "s|.*[^.]bashrc.*||g" ${HOME}/.bashrc
        sed -in "s|.*${SCRIPTDIR}/terminal/tmux/tmux\.conf.*||g" ${HOME}/.tmux.conf
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
        git config --global --unset core.excludesFile
        git config --global --unset core.attributesFile
        git config --global --unset include.path

        # Reset bash
        exec bash
        # Remove self
        #TODO remove comment to exit debugmode. Check that any changes are pushed...
        # rm -rf "${SCRIPTDIR}"
    fi
}

# {]} Uninstall

# {[} VSCode
vscodeExtensions() {
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
# {]} VSCode
installFonts() {
    mkdir -p "$FONTDIR"
    if [[ ! -d "${FONTDIR}/truetype/custom" ]]; then
        mkdir -p "${FONTDIR}/truetype/custom"
    fi

    printErr "Downloading fonts..."

    # # Get latest Iosevka font release.
    # fontUrl=`getLatestReleaseFileURL "be5invis/Iosevka" "iosevka-pack-[^z]*zip"`
    # fontdir="${FONTDIR}/Iosevka"
    # downloadURLAndExtractZipTo $fontUrl $fontdir && \
    #     printErr "${OK} Fonts installed to ${Yellow}${fontdir}${NC}" || \
    #     printErr "${Error} ${Red}Fonts failed to install to ${Yellow}${fontdir}${NC}"

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
}
#{]}

# {[} Vim
setupVim(){
    addTextIfAbsent "so $SCRIPTDIR/editors/vim/vimrc" "${HOME}/.vimrc"
    addTextIfAbsent "so $SCRIPTDIR/editors/vim/nvimrc" "${HOME}/config/nvim/init.vim"
    printErr "Installing vim plugins..."
    # Install Plug (plugin manager)
    downloadURLtoFile https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim "${HOME}/.vim/autoload/plug.vim"
    # This has the problem of making the caret disappear in WSL...
    vim -E +PlugInstall +qall
    # TODO: Saw this replicated elsewhere, but possibly without the -E. Hmmm...
    # Recover missing cursor due to previous command
    reset
}

installBrew() {
	if [[ $OSTYPE =~ 'darwin' ]]; then
		cd $HOME && mkdir -p homebrew && curl --insecure -L https://github.com/Homebrew/brew/tarball/master | tar xz --strip 1 -C homebrew
        HOMEBREW_PREFIX="$HOME/homebrew"
	else
		sh -c "$(curl -fsSL https://raw.githubusercontent.com/Linuxbrew/install/master/install.sh)"
        HOMEBREW_PREFIX="$HOME/.linuxbrew"
	fi
}

updateBrew() {
    brew="$HOMEBREW_PREFIX/bin/brew"
    "$brew" update
    # This reads words. Currently ok, but should be updated. TODO
    for i in $(cat "$SCRIPTDIR/system/Brewfile"); do "$brew" install "$i"; done
}

# {[} Git
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
            if [[ $REPLY =~ ^[0-9]+$ ]] && [[ $REPLY -gt 0 ]]; then
                break
            else
                printErr "${Red}Invalid input${NC}"
            fi
        done
        printErr "Git will remember your credentials for $REPLY minutes ($(( $REPLY * 60 )) seconds)."
        git config --global credential.helper "cache --timeout=$(( $REPLY * 60 ))"
    fi
}

setupSSH() {
    printErr "Enabling SSH config..."
    # Note: Includes only added since 7.3p1
    addTextIfAbsent "Include $DOTFILES_DIR/system/ssh/ssh_config" ${HOME}/.ssh/ssh_config
}

gitSettings() {
    printErr "Enabling custom git setup..."
    # Include is only supported on git versions > 1.7.10
    # (but 2.0 is quite standard anyway).
    # TODO check this, and also append contents if not.
    git config --global include.path ${SCRIPTDIR}/git/gitconfig
    git config --global core.excludesfile ${SCRIPTDIR}/git/gitignore
    git config --global core.attributesfile ${SCRIPTDIR}/git/gitattributes
}
# {]} Git

setupShell() {
    addTextIfAbsent ". $HOME/.bashrc" ${HOME}/.bash_profile
    printErr "Enabling custom bash setup..."
    addTextIfAbsent "source $SCRIPTDIR/bash/bashrc" ${HOME}/.bashrc
    downloadURLtoFile  \
        https://raw.githubusercontent.com/seebi/dircolors-solarized/master/dircolors.256dark  \
        "${HOME}/.dircolours_solarized"
    # https://raw.githubusercontent.com/seebi/dircolors-solarized/master/dircolors.ansi-universal

    printErr "Enabling custom tmux setup..."
    addTextIfAbsent "source-file $SCRIPTDIR/terminal/tmux/tmux.conf" ${HOME}/.tmux.conf
    printErr "Enabling custom readline (inputrc) setup..."
    addTextIfAbsent "\$include $SCRIPTDIR/bash/inputrc.sh" "${HOME}/.inputrc"
    printErr "Enabling custom X setup..."
    addTextIfAbsent "xrdb -merge \"$SCRIPTDIR/terminal/x/Xresources\"" "${HOME}/.Xresources"
    gitSettings
}

setupKitty() {
    addTextIfAbsent "include $SCRIPTDIR/terminal/kitty/kitty.conf" "${HOME}/.config/kitty/kitty.conf"
}
setupTermux() {
    mkdir -p "$HOME/.termux"
    downloadURLAndExtractZipTo "https://github.com/adi1090x/termux-style/raw/master/data.tar.gz" "$HOME/.termux/termux-style"
    cp "$HOME/.termux/termux-style/solarized-light.properties" "$HOME/.termux/"
}

readSettings() {
    if [ $ALL != 1 ]; then
        if askQuestionYN "Install VSCode extensions?" ; then
            doVS=1
        fi
        if askQuestionYN "Set up git credentials?" ; then
            doGit=1
        fi
        if askQuestionYN "Set up vim?" ; then
            doVim=1
        fi
        if askQuestionYN "Set up shell?" ; then
            doShell=1
        fi
        if askQuestionYN "Set up SSH?" ; then
            doSSSH=1
        fi
        if askQuestionYN "Set up terminal?" ; then
            doTerm=1
        fi
        if askQuestionYN "Install fonts?" ; then
            doFonts=1
        fi
        if askQuestionYN "Install brew?" ; then
            installBrew=1
            if askQuestionYN "Update brew and install Brewfile packages? (This can take a very long time)" ; then
                updateBrew=1
            fi
        fi
    fi
}

main() {
     #TODO: Make All an initial check, then case/if-nots for the rest.

    if [ ! $ALL = 1 ]; then
        readSettings
    fi

    if [[ $doVS == 1 ]] || [[ $ALL == 1 ]]; then
        printErr ""
        printErr '------------------- VSCODE EXTENSIONS'
        vscodeExtensions
    fi

    if [ $ALL = 1 ] || [ $doGit = 1 ]; then
        printErr ""
        printErr "------------------- GIT"
        gitUser ${1:-}
        gitCredentialCache
    fi

    if [ "$ALL" = 1 ] || [ "$doShell" = 1 ]; then
        printErr ""
        printErr "------------------- SHELL"
        setupShell
    fi

    if [ "$ALL" = 1 ] || [ "$doTerm" = 1 ]; then
        if substrInStr "kitty" "$TERM"; then
            printErr ""
            printErr "------------------- Kitty"
            setupKitty
        elif substrInStr "Android" "`uname -a`";  then
            printErr ""
            printErr "------------------- Termux"
            setupTermux
        fi
    fi

    if [ "$ALL" = 1 ] || [ "$doSSH" = 1 ]; then
        printErr ""
        printErr "------------------- SSH"
        setupSSH
    fi

    if [ "$ALL" = 1 ] || [ "$doFonts" = 1 ]; then
        printErr ""
        printErr "------------------- FONTS"
        installFonts
    fi

    if [ "$ALL" = 1 ] || [ "$doVim" = 1 ]; then
        printErr ""
        printErr "------------------- VIM"
        setupVim
    fi

    if [ "$ALL" = 1 ] || [ "$installBrew" = 1 ]; then
        printErr ""
        printErr "------------------- BREW INSTALL"
        installBrew
    fi

    if [ "$ALL" = 1 ] || [ "$updateBrew" = 1 ]; then
        printErr ""
        printErr "------------------- BREW UPDATE"
        updateBrew
    fi

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

if [[ $OSTYPE =~ 'linux' ]]; then

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
