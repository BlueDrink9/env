
{ pkgs, ... }:
{

  imports =
  [
    # (import (builtins.getFlake "github:xremap/nix-flake").nixosModules.default { system = "x86_64-linux"; })
    ./main_nongui.nix
    ./gui_essential.nix
    ./gui_extras.nix
    ./plasma.nix
    ./essential.nix
    ./sysadmin.nix
  ];
}
