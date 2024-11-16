{ config, pkgs, ... }:
{

  # for ddcutil to work
  hardware.i2c.enable = true;
  environment.systemPackages = with pkgs; [
    usbutils
    pciutils
    xdotool
    ddcutil
    exfatprogs
    exfat
  ];

}
