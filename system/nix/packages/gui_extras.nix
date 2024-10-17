{ config, pkgs, ... }:

{

  environment.systemPackages = with pkgs; [
    feh
    playerctl
    picom

    etesync-dav
    pandoc
    typst
    copyq
    brave
    pcloud
    megatools
    f3d
    espanso
    marimo

    qdirstat

    # xremap-flake.nixosModules.default
  ];

  programs.partition-manager.enable = true;

  programs.mepo.enable = true;

}
