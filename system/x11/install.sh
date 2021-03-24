#!/usr/bin/env bash
source "$DOTFILES_DIR/shell/script_functions.sh"

installID="X11"
installText=". \"$($SCRIPTDIR_CMD)/xinitrc\""
baseRC="${HOME}/.xinitrc"

eval "$(cat <<END
do${installID}() {
    printErr "Enabling custom ${installID} setup..."
    prependTextIfAbsent "${installText}" "${baseRC}"
    chmod u+x "${baseRC}"
    xsession_setup
    xresources_setup
  }
END
)"

eval "$(cat <<END
undo${installID}(){
    sed -in "s|.*${installText}.*||g" "${baseRC}"
  }
END
)"


installText=". \"$($SCRIPTDIR_CMD)/xinitrc\""
baseRC="${HOME}/.xsession"
eval "$(cat <<END
xsession_setup(){
    addTextIfAbsent "${installText}" "${baseRC}"
  }
END
)"

installText="#include <\"$($SCRIPTDIR_CMD)/xresources\">"
# installText="xrdb -merge \"$($SCRIPTDIR_CMD)/Xresources\""
baseRC="${HOME}/.Xresources"
eval "$(cat <<END
xresources_setup(){
    addTextIfAbsent "${installText}" "${baseRC}"
  }
END
)"


# If directly run instead of sourced, do all
if [ ! "${BASH_SOURCE[0]}" != "${0}" ]; then
  do${installID}
fi
