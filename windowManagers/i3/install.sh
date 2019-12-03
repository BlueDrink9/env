#!/usr/bin/env bash
source "$DOTFILES_DIR/shell/script_functions.sh"
installID="i3"
# installText="Include \"$($SCRIPTDIR_CMD)/ssh_config\""
configDir="${HOME}/.config/i3"
baseRC="${configDir}/config"

eval "$(cat <<END
do${installID}() {
    printErr "Enabling i3 config..."
    i3_CombineConf
}
END
)"
i3_CombineConf(){
    if [ ! -f "${baseRC}.pre-env" ]; then
        mv "${baseRC}" "${baseRC}.pre-env"
    else
        rm "${baseRC}"
    fi
    if [ ! -f "${configDir}/local_config" ]; then
        touch "${configDir}/local_config"
    fi
    cat "$($SCRIPTDIR_CMD)"/*.conf "${configDir}/local_config" > "${baseRC}"
}

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
