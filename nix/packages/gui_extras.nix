{ config, pkgs, ... }:

{

  my.pkgs = with pkgs; [
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
    marimo

    qdirstat

    copyq
    emacs
    thunderbird
    birdtray
    marimo

    gimp
    inkscape
    btop
    vscodium

    # xremap-flake.nixosModules.default
  ];

  programs.partition-manager.enable = true;

  programs.mepo.enable = true;

}
