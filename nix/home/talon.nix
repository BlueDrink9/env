{ lib, config, pkgs, ... }:
let
    talon_pkg = (builtins.getFlake "github:nix-community/talon-nix").packages.${builtins.currentSystem}.default;
  in
{

  home.packages = with pkgs; [
    talon_pkg
    mani
    # MS core fonts, needed for non-nixos linux?
    corefonts
    (python3.withPackages (python-pkgs: with python-pkgs; [
      pynvim
      neovim
      # gitman
    ]))
  ];
  fonts.fontconfig.enable = true;

  home.activation.talon = ''
    PATH=$PATH:${lib.makeBinPath [ pkgs.mani  pkgs.openssh ]}
    # Non-nixos git ssh settings may be funky.
    # https://github.com/NixOS/nixpkgs/issues/160527
    if [ ! -d /etc/nixos ]; then
      export GIT_SSH="/usr/bin/ssh"
    fi
    export talon_mani_config="${../../talon/talon_plugins_mani.yml}"
    export talon_user_dir="$HOME/.talon/user"
    mani sync --parallel --config $talon_mani_config
    mani run update --parallel --config $talon_mani_config
  '';
    # pushd ${../../talon}
    # ${gitman}/bin/gitman install

}
