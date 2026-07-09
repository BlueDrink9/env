{ config, pkgs, lib, ... }:

let
in
  {

  environment.systemPackages = with pkgs; [
    feh
    playerctl
    picom
    dunst
    albert
    xbacklight

  ];

}
