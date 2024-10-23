let
  dotfilesDir = builtins.getEnv "DOTFILES_DIR";
  user = builtins.getEnv "USER";
  nixDir = "${dotfilesDir}/system/nix";
in { config, pkgs, ... }:
{
  imports = [ <home-manager/nixos> ];

  # home-manager.users.${user} = {
  #   imports = [ "${nixDir}/home/home.nix" ];
  # };
  home-manager.useGlobalPkgs = true;
}
