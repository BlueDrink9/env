#!/usr/bin/env bash
# Source these for required functions and XDG variables (if used)
source "$DOTFILES_DIR/shell/script_functions.sh"
source "$DOTFILES_DIR/shell/functions.sh"
source "$DOTFILES_DIR/shell/XDG_setup.sh"

installID="Nixos"
installText=$(cat <<EOF
# let
# dotfilesDir = builtins.getEnv "DOTFILES_DIR";
# in { config, pkgs, ... }:

{
    imports =
    [
    ./hardware-configuration.nix
    $($SCRIPTDIR_CMD)/system/nix/configuration.nix
    ];
}
EOF
)
baseRC="/etc/nixos/configuration.nix"

eval "$(cat <<END
do${installID}() {
    printErr "Enabling custom ${installID} setup..."
    sudo echo "${installText}" >| "${baseRC}"
    # sudo addTextIfAbsent "${installText}" "${baseRC}"
    sudo nixos-rebuild switch
  }
END
)"

eval "$(cat <<END
undo${installID}(){
    sudo sed -in "s|.*${installText}.*||g" "${baseRC}"
  }
END
)"

# If directly run instead of sourced, do all
if [ ! "${BASH_SOURCE[0]}" != "${0}" ]; then
  do${installID}
fi
