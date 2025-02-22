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
    export talon_mani_config="${../../talon/talon_plugins_mani.yml}"
    export talon_user_dir="$HOME/.talon/user"
    ${mani}/bin/mani sync --parallel --config $talon_mani_config
    ${mani}/bin/mani run update --parallel --config $talon_mani_config
    # pushd ${../../talon}
    # ${gitman}/bin/gitman install
  '';

}
