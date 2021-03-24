#!/usr/bin/env bash
# Source these for required functions and XDG variables (if used)
source "$DOTFILES_DIR/shell/script_functions.sh"
source "$DOTFILES_DIR/shell/functions.sh"

source "$DOTFILES_DIR/shell/XDG_setup.sh"
installID="Polybar"
# installText="include-directory = $($SCRIPTDIR_CMD)"
installText="$(printf "[section/base]\ninclude-directory = $($SCRIPTDIR_CMD)/configs")"
# installText="$(printf "[section/base]\n"'include-directory = \${env:DOTFILES_DIR:fallback}/desktop_elements/polybar')"
baseRC="${XDG_CONFIG_HOME}/polybar/config.ini"
# installID installText baseRC

eval "$(cat <<END
do${installID}() {
    printErr "Enabling custom ${installID} setup..."
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
