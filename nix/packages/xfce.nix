{ config, pkgs, callPackage, lib, ... }:

{
  services.xserver = {
        enable = lib.mkDefault true;
        desktopManager = {
          xterm.enable = false;
          xfce = {
            enable = true;
            noDesktop = true;
            enableXfwm = false;
          };
        };
        windowManager.bspwm.enable = lib.mkDefault true;
      };
    services.displayManager.defaultSession = "xfce";

  # Enable the xfce Desktop Environment with bspwm.
  # TODO: Autostarts
  # TODO: timeout
}
