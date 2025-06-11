let
  dotfilesDir = builtins.getEnv "DOTFILES_DIR";
  user = builtins.getEnv "USER";
  nixDir = "${dotfilesDir}/system/nix";

  homeManagerUrl = {
    url = "https://github.com/nix-community/home-manager/archive/release-25.05.tar.gz";
    sha256 = "sha256:12246mk1xf1bmak1n36yfnr4b0vpcwlp6q66dgvz8ip8p27pfcw2";
  };

  can_import = builtins.tryEval(import <home-manager/nixos>);
  homeManagerPath = if can_import.success
    then <home-manager/nixos>
    else "${(builtins.fetchTarball homeManagerUrl)}/nixos";
  hm = import homeManagerPath;

in { config, pkgs, ... }:
{
  imports = [ hm ];

  # home-manager.users.${user} = {
  #   imports = [ "${nixDir}/home/home.nix" ];
  # };
  home-manager.useGlobalPkgs = true;
}
