# { config, pkgs, ... }:
# {
#   allowUnfree = true;
# }

let
unstableTarball =
fetchTarball
https://github.com/NixOS/nixpkgs/archive/nixos-unstable.tar.gz;
in
{
  nixpkgs.config = {
    allowUnfree = true;
    packageOverrides = pkgs: {
      unstable = import unstableTarball {
        config = config.nixpkgs.config;
      };
      myPackages = with pkgs; [
        brave
          veracrypt
          topgrade
          tdrop
          pcloud
          zoom-us
      ];
    };
  };
}
# environment.systemPackages = with pkgs; [
#   unstable.signal-desktop
# ];
