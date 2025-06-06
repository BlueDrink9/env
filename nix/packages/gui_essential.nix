{ config, pkgs, ... }:

{
  imports = [ ./firefox.nix ];

  environment.systemPackages = with pkgs; [
    bspwm
    sxhkd
    kitty
    xclip
    xsel
    workrave

    tdrop
    playerctl
    flameshot
    neovide
    unstable.vscodium
    espanso

    power-profiles-daemon

    # mosh
    # openvpn

    keepassxc
    joplin
    # Need Hunspell for libreoffice spellcheck, used for emacs too
    libreoffice-qt-fresh hunspell hunspellDicts.en_AU

    xorg.xev
    alsa-utils
  ];

  fonts.packages = with pkgs.nerd-fonts; [
    sauce-code-pro
    meslo-lg
    # san-franciso
  ];

}
