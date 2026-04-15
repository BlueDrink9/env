let

  homeManagerUrl = {
    url = "https://github.com/nix-community/home-manager/archive/release-25.11.tar.gz";
    sha256 = "sha256:0xpgskfs8q9jdd0hc8298h1qg2w6i36g0w1mmvyl169lmr8v3zqi";
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
