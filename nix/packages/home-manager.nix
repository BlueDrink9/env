let

  homeManagerUrl = {
    url = "https://github.com/nix-community/home-manager/archive/release-25.05.tar.gz";
    sha256 = "sha256:026rvynmzmpigax9f8gy9z67lsl6dhzv2p6s8wz4w06v3gjvspm1";
    # url = "https://github.com/nix-community/home-manager/archive/release-24.11.tar.gz";
    # sha256 = "sha256:1mwq9mzyw1al03z4q2ifbp6d0f0sx9f128xxazwrm62z0rcgv4na";
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
