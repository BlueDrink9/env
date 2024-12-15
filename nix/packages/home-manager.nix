let
  dotfilesDir = builtins.getEnv "DOTFILES_DIR";
  user = builtins.getEnv "USER";
  nixDir = "${dotfilesDir}/system/nix";

  homeManagerTarball = builtins.fetchTarball {
    url = "https://github.com/nix-community/home-manager/archive/release-24.11.tar.gz";
    sha256 = "17jgljy3c5v2nq5f5gy04xygbgjaaf1mrmcmrchfv3ypmq4mkgk5";
  };

  can_import = builtins.tryEval(import <home-manager/nixos>);
  homeManagerPath = if can_import.success
    then <home-manager/nixos>
    else "${homeManagerTarball}/nixos";
  hm = import homeManagerPath;

in { config, pkgs, ... }:
{
  imports = [ hm ];

  # home-manager.users.${user} = {
  #   imports = [ "${nixDir}/home/home.nix" ];
  # };
  home-manager.useGlobalPkgs = true;
}
