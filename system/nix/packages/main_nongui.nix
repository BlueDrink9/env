{ config, pkgs, ... }:

{

  programs.ydotool.enable = true;
  programs.kdeconnect.enable = true;
  programs.system-config-printer.enable = true;
  programs.adb.enable = true;
  programs.appimage = {
    enable = true;
    binfmt = true;
  };

  programs.direnv = {
    enable = true;
    direnvrcExtra = "echo Loaded direnv";
  };


  services.blueman.enable = true;

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
    bluetuith
    sqlite

    veracrypt
    syncthing
    topgrade

    # Easiest to just let Mason install (and hence trigger configuration for) some packages.
    # Mason needs these installers though.
    cargo

  ];

}
