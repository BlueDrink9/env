#!/usr/bin/env bash
# Source these for required functions and XDG variables (if used)
source "$DOTFILES_DIR/shell/script_functions.sh"
source "$DOTFILES_DIR/shell/functions.sh"
source "$DOTFILES_DIR/shell/XDG_setup.sh"

AddImport(){
    importFile=$1
    baseFile=$2
    lineNumber=$(awk '/imports =/ {print NR; exit}' "$baseFile")
    lineNumber=$(($lineNumber+2))
    SudoFunction prependTextIfAbsent "${importFile}" "${baseFile}" $lineNumber
}

installID="Nixos"
installTextFull=$(cat <<EOF
{ config, pkgs, ... }: {
    imports = [
        ./hardware-configuration.nix
        "$($SCRIPTDIR_CMD)/configuration.nix"
    ];
}
EOF
)
installText="\"$($SCRIPTDIR_CMD)/configuration.nix\""
baseRC="/etc/nixos/configuration.nix"

doNixos() {
    printErr "Enabling custom ${installID} setup..."
    if [ ! -f "${baseRC}" ]; then
        echo "${installTextFull}" | sudo tee "${baseRC}" > /dev/null
    else
        # TODO: is looking for imports too fragile? Could possibly use
        # hardware-configuration instead.
        AddImport "${installText}" "${baseRC}"
    fi
    echo "Building nix; may take a long time!"
    sudo nixos-rebuild switch


    flake=$(cat <<EOF
    {
        description = "nixos";
        inputs = {
            nixpkgs.url = "github:nixos/nixpkgs/nixos-25.05";
        };

        outputs = {
            self,
            nixpkgs,
            ...
        } @ inputs: let
            inherit (self) outputs;
        in {
            # NixOS configuration entrypoint
            # Available through 'nixos-rebuild --flake .#your-hostname'
            nixosConfigurations = {
            $HOST = nixpkgs.lib.nixosSystem {
                specialArgs = {inherit inputs outputs;};
                # > Our main nixos configuration file <
                modules = [./configuration.nix];
            };
            };
        };
    }
EOF
    )

    echo $flake | sudo tee /etc/nixos/flake.nix

}

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
