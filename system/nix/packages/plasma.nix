{ config, pkgs, ... }:

{
  # Enable the KDE Plasma Desktop Environment.
  # TODO: Autostarts
  # TODO: timeout
  services.displayManager.sddm.enable = true;
  services.desktopManager.plasma6.enable = true;
  services.displayManager.defaultSession = "plasmax11";

  environment.systemPackages = with pkgs; [
    kdePackages.ksshaskpass
  ];

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
