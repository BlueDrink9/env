{ config, pkgs, ... }:

{

  environment.systemPackages = with pkgs; [
    bspwm
    sxhkd
    kitty
    xclip
    xsel
    workrave

    btop
    # mosh
    openvpn
    # Github cli, for easy auth. gh auth && gh auth setup-git
    gh
    git-credential-manager
    lazygit
    kdePackages.kconfig

    shellcheck
    pyright
    nil # Nix lsp

    emacs
    vscodium
    thunderbird
    birdtray
    etesync-dav
    keepassxc
    pandoc
    typst
    copyq
    inkscape
    gimp
    zotero
    joplin
    signal-desktop
    neovide
    brave
    pcloud
    megatools
    f3d
    # Need Hunspell for libreoffice spellcheck, used for emacs too
    libreoffice-qt-fresh hunspell hunspellDicts.en_AU
    espanso
    marimo

    picom
    feh
    playerctl
    heroic

  ];

}
