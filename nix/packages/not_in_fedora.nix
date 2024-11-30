{ pkgs, ... }:
  # Use home-manager for these packages because they often
  # aren't in mainstream repos (at least, this list isn't on
  # Fedora)
let
  unstable = import <nixpkgs-unstable> {};
in
{
  environment.systemPackages = with pkgs; [
    # (pkgs.nerdfonts.override { fonts = [ "FantasqueSansMono" ]; })
    veracrypt
    tdrop

    birdtray
    unstable.brave
    duplicacy
    element
    espanso
    fscryptctl
    pcloud
    signal-desktop
    tldr
    topgrade
    ventoy
    vivid
    vscodium
    zotero
    caprine-bin
    zapzap
    # TODO San francisco fonts
    # https://github.com/Lyndeno/apple-fonts.nix

    # albert, ulauncher, dlauncher?
  ];
}
