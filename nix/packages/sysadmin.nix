{ config, pkgs, ... }:
{

  # Index provides command-not-found functionality
  programs.command-not-found.enable = false;
  programs.nix-index.enable = true;

  # for ddcutil to work
  hardware.i2c.enable = true;
  environment.systemPackages = with pkgs; [
    usbutils
    pciutils
    xdotool
    ddcutil
    exfatprogs
    exfat
    hwinfo
    wirelesstools
    cpuinfo
  ];

}
