#!/usr/bin/env bash
source "$DOTFILES_DIR/bash/script_functions.sh"
installID="i3"
# installText="Include \"$($SCRIPTDIR_CMD)/ssh_config\""
configDir="${HOME}/.config/i3"
baseRC="${configDir}/config"

eval "$(cat <<END
do${installID}() {
    printErr "Enabling i3 config..."
    if [ ! -f "${baseRC}.pre-env" ]; then
        mv "${baseRC}" "${baseRC}.pre-env"
    else
        rm "${baseRC}"
    fi
    if [ ! -f "${configDir}/local_config" ]; then
        touch "${configDir}/local_config"
    fi
    cat "${DOTFILES_DIR}"/windowManagers/i3/*.conf "${configDir}/local_config" > "${baseRC}"
}
END
)"

eval "$(cat <<END
undo${installID}(){
    mv "${baseRC}" "${baseRC}.pre-env"
}
END
)"

# If directly run instead of sourced, do all
if [ ! "${BASH_SOURCE[0]}" != "${0}" ]; then
    do${installID}
fi
