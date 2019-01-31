#!/usr/bin/env bash
source "$DOTFILES_DIR/bash/script_functions.sh"
installID="bspwm"
installText=". \"$($SCRIPTDIR_CMD)/bspwmrc\""
configDir="${HOME}/.config/bspwm"
baseRC="${configDir}/bspwmrc"

eval "$(cat <<END
do${installID}() {
    printErr "Enabling bspwm config..."
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
