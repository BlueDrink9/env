{ config, pkgs, ... }:
{

  my.pkgs = with pkgs; [
  # for ddcutil to work
  hardware.i2c.enable = true;
    usbutils
    pciutils
    xdotool
    ddcutil
  ];

}
