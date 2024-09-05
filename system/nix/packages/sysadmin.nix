{ config, pkgs, ... }:
{

  environment.systemPackages = with pkgs; [
    usbutils
    pciutils
  ];

}
