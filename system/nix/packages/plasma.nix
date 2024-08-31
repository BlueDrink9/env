{ config, pkgs, ... }:

{
  # Enable the KDE Plasma Desktop Environment.
  # TODO: Autostarts
  # TODO: timeout
  services.displayManager.sddm.enable = true;
  services.desktopManager.plasma6.enable = true;
  services.displayManager.defaultSession = "plasmax11";

  qt = {
    enable = true;
    platformTheme = "kde";
    style = "breeze";
  };

  # system.activationScripts.x = {
  #   text = ''
  #   '';
  # };

}
