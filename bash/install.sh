#!/usr/bin/env bash
source "$DOTFILES_DIR/bash/script_functions.sh"

installID="shell"
installText="source $($SCRIPTDIR_CMD)/bashrc"
baseRC="${HOME}/.bashrc"

eval "$(cat <<END
do${installID}() {
    printErr "Enabling custom bash setup..."
    addTextIfAbsent "source $HOME/.bashrc" "${HOME}/.bash_profile"
    addTextIfAbsent "${installText}" "${baseRC}"

    printErr "Downloading dircolours_solarized..."
    downloadURLtoFile  \
        https://raw.githubusercontent.com/seebi/dircolors-solarized/master/dircolors.256dark  \
        "${HOME}/.dircolours_solarized"
    # https://raw.githubusercontent.com/seebi/dircolors-solarized/master/dircolors.ansi-universal

    printErr "Enabling custom readline (inputrc) setup..."
    addTextIfAbsent "\$include $($SCRIPTDIR_CMD)/inputrc.sh" "${HOME}/.inputrc"
    gitSettings

}
END
)"

eval "$(cat <<END
undo${installID}(){
    # The sed commands replace source lines with blanks
    sed -in "s|.*${installText}.*||g" "${baseRC}"
    sed -in "s|source $HOME/.bashrc||g" "${HOME}/.bash_profile"
    rm -rf "${HOME}/.dircolours_solarized"
    sed -in "s|.*$($SCRIPTDIR_CMD)/inputrc.sh.*||g" "${HOME}/.inputrc"
}
END
)"

# If directly run instead of sourced, do all
if [ ! "${BASH_SOURCE[0]}" != "${0}" ]; then
    do${installID}
fi
