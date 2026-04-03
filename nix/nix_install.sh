#!/bin/sh

nix_install(){
  if command -v nix > /dev/null 2>&1; then
    return
  fi
  # Nix installer will auto-cd to tmp and not return, so saving cwd here.
  pushd "." > /dev/null
  echo "Installing nix with Determinate installer (needs sudo)."
  cd "$(mktemp -d)" || exit
  curl -sL -o nix-installer https://github.com/DeterminateSystems/nix-installer/releases/latest/download/nix-installer-x86_64-linux
  chmod +x nix-installer
  sudo ./nix-installer install --no-confirm
  . /nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh
  popd > /dev/null
}

