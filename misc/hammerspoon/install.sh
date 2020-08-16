#!/usr/bin/env bash
source "$DOTFILES_DIR/shell/script_functions.sh"
source "$DOTFILES_DIR/shell/functions.sh"

installID="Hammerspoon"
installText="dofile('$($SCRIPTDIR_CMD)/init.lua')"
baseRC="${HOME}/.hammerspoon/init.lua"

eval "$(cat <<END
do${installID}() {
    printErr "Enabling hammerspoon setup..."
    addTextIfAbsent "${installText}" "${baseRC}"
    downloadSpoons
  }
END
)"

eval "$(cat <<END
undo${installID}(){
    sed -in "s|.*${installText}.*||g" "${baseRC}"
  }
END
)"

downloadSpoons(){
  mkdir -p ~/.hammerspoon/Spoons
  pushd ~/.hammerspoon/Spoons
  if [ -d VimMode.spoon ]; then
    git -C VimMode.spoon pull
  else
    git clone --depth=1 https://github.com/dbalatero/VimMode.spoon
  fi
  popd
}

# If directly run instead of sourced, do all
if [ ! "${BASH_SOURCE[0]}" != "${0}" ]; then
  doHammerspoon
fi
