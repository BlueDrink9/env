let
  dotfilesDir = builtins.getEnv "DOTFILES_DIR";
  user = builtins.getEnv "USER";
  nixDir = "${dotfilesDir}/system/nix";

  homeManagerTarball = builtins.fetchTarball {
    url = "https://github.com/nix-community/home-manager/archive/release-24.11.tar.gz";
    sha256 = "15k41il0mvmwyv6jns4z8k6khhmb22jk5gpcqs1paym3l01g6abn";
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
