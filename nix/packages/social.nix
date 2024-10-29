{ config, pkgs, ... }:
{

  my.pkgs = with pkgs; [
    signal-desktop
    zapzap
    caprine-bin
  ];

}
