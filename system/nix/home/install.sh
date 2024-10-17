#!/usr/bin/env bash
# Source these for required functions and XDG variables (if used)
source "$DOTFILES_DIR/shell/script_functions.sh"
source "$DOTFILES_DIR/shell/functions.sh"
source "$DOTFILES_DIR/shell/XDG_setup.sh"
# nix has a fit if tmp dir has a trailing slash
export TMPDIR="${TMPDIR%/}"

nix_install(){
  cd "$(mktemp -d)" || exit
  curl -sL -o nix-installer https://github.com/DeterminateSystems/nix-installer/releases/latest/download/nix-installer-x86_64-linux
  chmod +x nix-installer
  sudo ./nix-installer install --no-confirm
}

add_channels(){
  nix-channel --add https://github.com/nix-community/home-manager/archive/master.tar.gz home-manager
  nix-channel --add https://github.com/nix-community/plasma-manager/archive/trunk.tar.gz plasma-manager
  nix-channel --add https://nixos.org/channels/nixpkgs-unstable unstable

  if [ ! -d /etc/nixos ]; then
    nix-channel --add https://nixos.org/channels/nixpkgs-unstable nixpkgs
  fi

  nix-channel --update
}

home_manager_install(){
  nix-shell '<home-manager>' -A install
  installText=". $HOME/.nix-profile/etc/profile.d/hm-session-vars.sh"
  addTextIfAbsent "${installText}" "$HOME/.bashrc"
  addTextIfAbsent "${installText}" "$HOME/.zshrc"

  if [ ! -d /etc/NIXOS ]; then
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

doHomeManager(){
  if [ ! -d /etc/nixos ]; then
    nix_install
  fi
  add_channels
  home_manager_install
}

if [ ! "${BASH_SOURCE[0]}" != "${0}" ]; then
  doHomeManager
fi
