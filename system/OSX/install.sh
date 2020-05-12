#!/usr/bin/env bash
source "$DOTFILES_DIR/shell/script_functions.sh"
source "$DOTFILES_DIR/shell/functions.sh"

installID="OSX"
# installText="source-file $($SCRIPTDIR_CMD)/tmux/tmux.conf"
# baseRC="${HOME}/.tmux.conf"

eval "$(cat <<END
do${installID}() {
    printErr "Enabling custom OSX settings..."
    source "$($SCRIPTDIR_CMD)/setup.sh"
    addTextIfAbsent "dofile(\"$DOTFILES_DIR/misc/hammerspoon/init.lua\")" ~/.hammerspoon/init.lua
  }
END
)"

eval "$(cat <<END
undo${installID}(){
    echo "No undo avaiable for osx settings. Reset manually or create a new user"
  }
END
)"

# If directly run instead of sourced, do all
if [ ! "${BASH_SOURCE[0]}" != "${0}" ]; then
  doOSX
fi
