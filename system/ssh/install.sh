#!/usr/bin/env bash
source "$DOTFILES_DIR/bash/script_functions.sh"
installID="SSH"
installText="Include \"$($SCRIPTDIR_CMD)/ssh_config\""
baseRC="${HOME}/.ssh/ssh_config"

eval "$(cat <<END
do${installID}() {
    printErr "Enabling SSH config..."
    # Note: Includes only added since 7.3p1

    addTextIfAbsent "${installText}" "${baseRC}"
    for key in $($SCRIPTDIR_CMD)/authorized_keys/*; do
        key="$(cat $key)"
        addTextIfAbsent "$key" "${HOME}/.ssh/authorized_keys"
    done

}
END
)"

eval "$(cat <<END
undo${installID}(){
    sed -in "s|.*${installText}.*||g" "${baseRC}"

    for key in $($SCRIPTDIR_CMD)/authorized_keys/*; do
        key="$(cat $key)"
        sed -in "s|$key||g" "${HOME}/.ssh/authorized_keys"
    done
}
END
)"

# If directly run instead of sourced, do all
if [ ! "${BASH_SOURCE[0]}" != "${0}" ]; then
    do${installID}
fi
