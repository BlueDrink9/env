
{ pkgs, ... }:
{

  imports =
  [
    # (import (builtins.getFlake "github:xremap/nix-flake").nixosModules.default { system = "x86_64-linux"; })
    ./main_nongui.nix
    ./gui.nix
    ./plasma.nix
    ./essential.nix
    ./sysadmin.nix
  ];
}
