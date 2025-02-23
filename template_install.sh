#!/usr/bin/env bash
# Source these for required functions and XDG variables (if used)
source "$DOTFILES_DIR/shell/script_functions.sh"
source "$DOTFILES_DIR/shell/functions.sh"

source "$DOTFILES_DIR/shell/XDG_setup.sh"
installID="Prog"
installText="source-file $($SCRIPTDIR_CMD)/prog/prog.conf"
# installText="$(printf 'source(\\"%s\\")' "$($SCRIPTDIR_CMD)/prog.R")"
baseRC="${XDG_CONFIG_HOME}/prog/.prog.conf"

source "$DOTFILES_DIR/generic_rc_installer.sh"

# If directly run instead of sourced, do all
if [ ! "${BASH_SOURCE[0]}" != "${0}" ]; then
  do${installID}
fi
