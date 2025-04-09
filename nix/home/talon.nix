{ lib, config, pkgs, ... }:
let
    talon_pkg = (builtins.getFlake "github:nix-community/talon-nix").packages.${builtins.currentSystem}.default;
  talon_beta = (
      builtins.getFlake "git+file://${config.home.homeDirectory}/src/talon-nix"
    ).packages.${builtins.currentSystem}.default;

  in
{

  home.packages = with pkgs; [
    talon_beta
    # talon_pkg
    mani
    # MS core fonts, needed for non-nixos linux?
    corefonts
    # (python3.withPackages (python-pkgs: with python-pkgs; [
    #   pynvim
    #   neovim
    #   # gitman
    # ]))
  ];
  fonts.fontconfig.enable = true;

  home.activation."talon python packages" = ''
    ~/.talon/bin/python -m pip install pynvim
  '';

  home.activation."talon user repos" = ''
    PATH=$PATH:${lib.makeBinPath [ pkgs.mani  pkgs.openssh ]}
    # Non-nixos git ssh settings may be funky.
    # https://github.com/NixOS/nixpkgs/issues/160527
    if [ ! -d /etc/nixos ]; then
      export GIT_SSH="/usr/bin/ssh"
    fi
    . ${../../talon/sync_plugins.sh}"
  '';

}
