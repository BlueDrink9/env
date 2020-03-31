#!/usr/bin/env bash
source "$DOTFILES_DIR/shell/script_functions.sh"
source "$DOTFILES_DIR/shell/XDG_setup.sh"
installID="chunkwm"
chunkwmDir="$(resolveSymlinkToDir $(which chunkwm))/.."
chunkwmDir="${chunkwmDir:-$HOMEBREW_PREFIX/opt/chunkwm}"
installText="chunkc core::plugin_dir $chunkwmDir/share/chunkwm/plugins; source \"$($SCRIPTDIR_CMD)/chunkwmrc\""
configDir="${XDG_CONFIG_HOME}/chunkwm"
baseRC="${configDir}/chunkwmrc"

eval "$(cat <<END
do${installID}() {
    printErr "Enabling chunkwm & skhd config..."
    addTextIfAbsent "${installText}" "${baseRC}"
    chmod +x "${baseRC}"
    doSkhd
    getFloatScripts
  }
END
)"
getFloatScripts() {
  downloadURLtoFile https://raw.githubusercontent.com/Amar1729/dotfiles/407803a1de201dc63c57a9d4ee5bc23c09bcdb20/.config/scripts/chunk_float.sh ~/.config/scripts/chunk_float.sh
  # downloadURLtoFile https://github.com/Amar1729/dotfiles/blob/407803a1de201dc63c57a9d4ee5bc23c09bcdb20/.config/scripts/chunk_resize_mgr.sh chunk_resize_mgr.sh
}

doSkhd(){
  installText=".load \"$($SCRIPTDIR_CMD)/skhd/skhdrc\""
  baseRC="$XDG_CONFIG_HOME/skhd/skhdrc"
  addTextIfAbsent "${installText}" "${baseRC}"
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
