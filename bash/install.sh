#!/usr/bin/env bash
source "$DOTFILES_DIR/bash/script_functions.sh"

doShell() {
    printErr "Enabling custom bash setup..."
    addTextIfAbsent "source $HOME/.bashrc" "${HOME}/.bash_profile"
    addTextIfAbsent "source $($SCRIPTDIR_CMD)/bashrc" "${HOME}/.bashrc"

    printErr "Downloading dircolours_solarized..."
    downloadURLtoFile  \
        https://raw.githubusercontent.com/seebi/dircolors-solarized/master/dircolors.256dark  \
        "${HOME}/.dircolours_solarized"
    # https://raw.githubusercontent.com/seebi/dircolors-solarized/master/dircolors.ansi-universal

    printErr "Enabling custom readline (inputrc) setup..."
    addTextIfAbsent "\$include $($SCRIPTDIR_CMD)/inputrc.sh" "${HOME}/.inputrc"
    gitSettings
}

undoShell(){
    # The sed commands replace source lines with blanks
    sed -in "s|.*$($SCRIPTDIR_CMD)/bashrc.*||g" "${HOME}/.bashrc"
    sed -in "s|source $HOME/.bashrc||g" "${HOME}/.bash_profile"
    rm -rf "${HOME}/.dircolours_solarized"
    sed -in "s|.*$($SCRIPTDIR_CMD)/inputrc.sh.*||g" "${HOME}/.inputrc"
}


# If directly run instead of sourced, do all
if [ ! "${BASH_SOURCE[0]}" != "${0}" ]; then
    setupShell
fi
