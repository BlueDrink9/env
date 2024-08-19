{ config, pkgs, ... }:

{

  environment.systemPackages = with pkgs; [
    libgcc
    gnumake
    cmake
    automake
    pkg-config

    universal-ctags
    ncurses
    bat-extras.batgrep
    cheat
    tldr
    broot
    duf
    mcfly
    thefuck

    veracrypt
    syncthing
    topgrade

    # Easiest to just let Mason install (and hence trigger configuration for) some packages.
    # Mason needs these installers though.
    cargo

  ];

}
