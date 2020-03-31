#!/usr/bin/env bash
source "$DOTFILES_DIR/shell/script_functions.sh"
source "$DOTFILES_DIR/shell/XDG_setup.sh"
installID="Yabai"
configDir="${XDG_CONFIG_HOME}/yabai"
installText="source \"$($SCRIPTDIR_CMD)/yabairc\""
baseRC="${configDir}/yabairc"

eval "$(cat <<END
do${installID}() {
    printErr "Enabling yabai & skhd config..."
    addTextIfAbsent "${installText}" "${baseRC}"
    chmod +x "${baseRC}"
    doSkhd
    getFloatScripts
    fixTilingFolderFromDesktop
  }
END
)"

eval "$(cat <<END
undo${installID}(){
    mv "${baseRC}" "${baseRC}.pre-env"
  }
END
)"

getFloatScripts() {
  downloadURLtoFile https://raw.githubusercontent.com/Amar1729/dotfiles/407803a1de201dc63c57a9d4ee5bc23c09bcdb20/.config/scripts/chunk_float.sh ~/.config/scripts/chunk_float.sh
  # downloadURLtoFile https://github.com/Amar1729/dotfiles/blob/407803a1de201dc63c57a9d4ee5bc23c09bcdb20/.config/scripts/chunk_resize_mgr.sh chunk_resize_mgr.sh
}

fixTilingFolderFromDesktop(){
  defaults write com.apple.finder DisableAllAnimations -bool true
  killall Finder # or logout and login
  # to reset system defaults, delete the key instead
  # defaults delete com.apple.finder DisableAllAnimations
}

doSkhd(){
  installText=".load \"$($SCRIPTDIR_CMD)/skhd/skhdrc\""
  baseRC="$XDG_CONFIG_HOME/skhd/skhdrc"
  addTextIfAbsent "${installText}" "${baseRC}"
}

# If directly run instead of sourced, do all
if [ ! "${BASH_SOURCE[0]}" != "${0}" ]; then
  do${installID}
fi
