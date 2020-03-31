#!/usr/bin/env bash
source "$DOTFILES_DIR/shell/script_functions.sh"
source "$($SCRIPTDIR_CMD)/bspwm/install.sh"
source "$($SCRIPTDIR_CMD)/chunkwm/install.sh"
source "$($SCRIPTDIR_CMD)/yabai/install.sh"

installID="WM"

eval "$(cat <<END
do${installID}() {
  printErr "Enabling WM setup..."

  if [ "$OSTYPE" = "linux-gnu" ]; then
    doBspwm
  elif [[ $OSTYPE =~ 'darwin' ]]; then
    doYabai
  fi

}
END
)"

eval "$(cat <<END
undo${installID}(){
  if [ "$OSTYPE" = "linux-gnu" ]; then
    undoBspwm
  elif [[ $OSTYPE =~ 'darwin' ]]; then
    undoYabai
  fi
}
END
)"

# If directly run instead of sourced, do all
if [ ! "${BASH_SOURCE[0]}" != "${0}" ]; then
  doWM
fi
