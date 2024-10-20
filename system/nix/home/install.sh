#!/usr/bin/env bash
# Source these for required functions and XDG variables (if used)
source "$DOTFILES_DIR/shell/script_functions.sh"
source "$DOTFILES_DIR/shell/functions.sh"
source "$DOTFILES_DIR/shell/XDG_setup.sh"
source "$($SCRIPTDIR_CMD)/../install.sh"
# nix has a fit if tmp dir has a trailing slash
export TMPDIR="${TMPDIR%/}"

if [ ! -d /etc/nixos ]; then
  version="24.11"
else
  version="$(nixos-version | cut -d. -f1,2)"
fi

nix_install(){
  echo "Installing nix with Determinate installer (needs sudo)."
  cd "$(mktemp -d)" || exit
  curl -sL -o nix-installer https://github.com/DeterminateSystems/nix-installer/releases/latest/download/nix-installer-x86_64-linux
  chmod +x nix-installer
  sudo ./nix-installer install --no-confirm
  . /nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh
}

if [ ! -d /etc/nixos ]; then
  version="24.11"
else
  version="$(nixos-version | cut -d. -f1,2)"
fi

add_channels() {
  echo "Setting nix channels."
  declare -A channels
  channels["plasma-manager"]="https://github.com/nix-community/plasma-manager/archive/trunk.tar.gz"
  channels["nixpkgs-unstable"]="https://nixos.org/channels/nixpkgs-unstable"

  if [ -d /etc/nixos ]; then
    # NixOS: use specific version
    channels["nixpkgs"]="https://github.com/nixos/nixpkgs/archive/release-$version.tar.gz"
    channels["home-manager"]="https://github.com/nix-community/home-manager/archive/release-$version.tar.gz"
  else
    # Non-NixOS: use latest channels
    channels["nixpkgs"]=${channels["nixpkgs-unstable"]}
    channels["home-manager"]="https://github.com/nix-community/home-manager/archive/master.tar.gz"
  fi

  for channel in "${!channels[@]}"; do
    if [ -d /etc/nixos ]; then
      sudo nix-channel --add "${channels[$channel]}" "$channel"
    fi
    nix-channel --add "${channels[$channel]}" "$channel"
  done

  if [ -d /etc/nixos ]; then
    sudo nix-channel --update
  fi
  nix-channel --update
}

home_manager_install(){
  if [ ! -d /etc/nixos ]; then
    nix-shell '<home-manager>' -A install
  else
    # Add nixos hm setup config to imports list
    baseRC="/etc/nixos/configuration.nix"
    import="$(realpath "$($SCRIPTDIR_CMD)/../packages/home-manager.nix")"
    AddImport "${import}" "${baseRC}"
  fi
  installText=". $HOME/.nix-profile/etc/profile.d/hm-session-vars.sh"
  addTextIfAbsent "${installText}" "$HOME/.bashrc"
  addTextIfAbsent "${installText}" "$HOME/.zshrc"

  if [ ! -d /etc/nixos ]; then
    generic="targets.genericLinux.enable = true;"
  fi
  # installID="HomeManager"
  installTextFull=$(cat <<EOF
  { config, pkgs, ... }: {
    imports =
    [
    "$($SCRIPTDIR_CMD)/home.nix"
    ./local-packages.nix
    ];

    $generic
    home.username = "$USER";
    home.homeDirectory = "$HOME";
    home.stateVersion = "$version";
  }
EOF
  )
  baseRC="$XDG_CONFIG_HOME/home-manager/home.nix"
  if [ ! -f "${baseRC}" ]; then
    addTextIfAbsent "${installTextFull}" "${baseRC}"
  fi

  # installID="HomeManager"
  installTextFull=$(cat <<EOF
  { pkgs, ... }:
    {
      home.packages = with pkgs; [
      ];
    }
EOF
  )
  baseRC="$XDG_CONFIG_HOME/home-manager/local-packages.nix"
  if [ ! -f "${baseRC}" ]; then
    addTextIfAbsent "${installTextFull}" "${baseRC}"
  fi
}

doHomeManagerPackages(){
  pkgSets="essential main_nongui sysadmin"
  if [ "${GUISYSTEM}" = 1 ]; then
    pkgSets="$pkgSets guiEssential guiExtras plasma"
  fi
  baseRC="$XDG_CONFIG_HOME/home-manager/home.nix"
  for set in $pkgSets; do
    file="$($SCRIPTDIR_CMD)/../packages/${set}.nix"
    AddImport "${file}" "${baseRC}"
  done
}


doHomeManager(){
  if [ ! -d /etc/nixos ]; then
    nix_install
  fi
  add_channels
  home_manager_install
  nix-shell -p home-manager --command "home-manager switch"
}

if [ ! "${BASH_SOURCE[0]}" != "${0}" ]; then
  doHomeManager
fi
