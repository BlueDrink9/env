#!/usr/bin/env bash
source "$DOTFILES_DIR/bash/script_functions.sh"

installID="X"
installText="xrdb -merge \"$($SCRIPTDIR_CMD)/.xinitrc\""
baseRC="${HOME}/.xinitrc"

eval "$(cat <<END
do${installID}() {
    printErr "Enabling i3 Xinit..."
    addTextIfAbsent "${installText}" "${baseRC}"

}
END
)"

eval "$(cat <<END
undo${installID}(){
    sed -in "s|.*${installText}.*||g" "${baseRC}"
}
END
)"

# If directly run instead of sourced, do all
if [ ! "${BASH_SOURCE[0]}" != "${0}" ]; then
    do${installID}
fi
