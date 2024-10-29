{ config, pkgs, ... }:
{

  my.pkgs = with pkgs; [
    usbutils
    pciutils
    xdotool
  ];

}
