#!/usr/bin/env bash
# Source these for required functions and XDG variables (if used)
source "$DOTFILES_DIR/shell/script_functions.sh"
source "$DOTFILES_DIR/shell/functions.sh"

source "$DOTFILES_DIR/shell/XDG_setup.sh"

# Can't use generic installer because heredoc breaks quoting.
doRofi() {
  installText='@import "'$($SCRIPTDIR_CMD)'/rofi/config"'
  baseRC="${XDG_CONFIG_HOME}/rofi/config.rasi"
  printErr "Enabling custom Rofi setup..."
  addTextIfAbsent "${installText}" "${baseRC}"
}

doPlasma() {
  "$($SCRIPTDIR_CMD)/install_fakwin.sh"
  "$($SCRIPTDIR_CMD)/plasma_setup.sh"
}


# source "$DOTFILES_DIR/generic_rc_installer.sh"

# If directly run instead of sourced, do all
if [ ! "${BASH_SOURCE[0]}" != "${0}" ]; then
  # do${installID}
  # doRofi
  doPlasma
fi
