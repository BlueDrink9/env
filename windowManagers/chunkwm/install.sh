#!/usr/bin/env bash
source "$DOTFILES_DIR/bash/script_functions.sh"
installID="chunkwm"
installText="chunkc core::plugin_dir $HOMEBREW_PREFIX/opt/chunkwm/share/chunkwm/plugins; source \"$($SCRIPTDIR_CMD)/chunkwmrc\""
configDir="${HOME}"
baseRC="${configDir}/.chunkwmrc"

eval "$(cat <<END
do${installID}() {
    printErr "Enabling chunkwm & skhd config..."
    addTextIfAbsent "${installText}" "${baseRC}"
    skhd_CombineConf
}
END
)"
skhd_CombineConf(){
    local baseRC="${configDir}/.skhdrc"
    if [ ! -f "${baseRC}.pre-env" ]; then
        mv "${baseRC}" "${baseRC}.pre-env"
    else
        rm "${baseRC}"
    fi
    if [ ! -f "${configDir}/local_config" ]; then
        touch "${configDir}/local_config"
    fi
    cat "$($SCRIPTDIR_CMD)"/skhd/* "${configDir}/local_config" > "${baseRC}"
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
