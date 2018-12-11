#!/usr/bin/env bash
SCRIPTDIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
echo $SCRIPTDIR

doShell() {
    printErr "Enabling custom bash setup..."
    addTextIfAbsent "source $HOME/.bashrc" "${HOME}/.bash_profile"
    addTextIfAbsent "source $SCRIPTDIR/bashrc" "${HOME}/.bashrc"

    printErr "Downloading dircolours_solarized..."
    downloadURLtoFile  \
        https://raw.githubusercontent.com/seebi/dircolors-solarized/master/dircolors.256dark  \
        "${HOME}/.dircolours_solarized"
    # https://raw.githubusercontent.com/seebi/dircolors-solarized/master/dircolors.ansi-universal

    printErr "Enabling custom readline (inputrc) setup..."
    addTextIfAbsent "\$include $SCRIPTDIR/inputrc.sh" "${HOME}/.inputrc"
    gitSettings
}

undoShell(){
    # The sed commands replace source lines with blanks
    sed -in "s|.*${SCRIPTDIR}/bashrc.*||g" "${HOME}/.bashrc"
    sed -in "s|source $HOME/.bashrc||g" "${HOME}/.bash_profile"
    rm -rf "${HOME}/.dircolours_solarized"
    sed -in "s|.*${SCRIPTDIR}/inputrc.sh.*||g" "${HOME}/.inputrc"
}


# If interactive, do all
if [[ $- == *i* ]]; then
    setupShell
fi
