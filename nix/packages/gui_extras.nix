{ config, pkgs, ... }:

let
  nixgl = (builtins.getFlake "github:nix-community/nixGL").packages.${builtins.currentSystem}.default;
in {

  environment.systemPackages = with pkgs; [
    # Run opengl apps on nixos: nixGL <program>
    nixgl
    feh
    playerctl
    picom
    libnotify

    etesync-dav
    pandoc
    typst
    copyq
    unstable.brave
    unstable.pcloud
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
