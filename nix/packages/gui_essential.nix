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
    espanso

    # mosh
    # openvpn

    keepassxc
    joplin
    # Need Hunspell for libreoffice spellcheck, used for emacs too
    libreoffice-qt-fresh hunspell hunspellDicts.en_AU

    xorg.xev
    alsa-utils
  ];

  fonts.packages = with pkgs; [
    # source-code-pro
    (nerdfonts.override { fonts = [ "SourceCodePro" "Meslo" ]; })
    # menlo
    # san-franciso
  ];

}
