let
  dotfilesDir = builtins.getEnv "DOTFILES_DIR";
  user = builtins.getEnv "USER";
  nixDir = "${dotfilesDir}/system/nix";

  homeManagerTarball = builtins.fetchTarball {
    url = "https://github.com/nix-community/home-manager/archive/release-24.05.tar.gz";
    sha256 = "00wp0s9b5nm5rsbwpc1wzfrkyxxmqjwsc1kcibjdbfkh69arcpsn";
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
