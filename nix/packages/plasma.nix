{ config, pkgs, lib, ... }:

{

  # Backport fix for bspwm to versions before 6.6. Not necessary from 26.05 onwards.
  nixpkgs.overlays = if (lib.trivial.version == "25.05") then [
    (final: prev: {
      kdePackages = prev.kdePackages.overrideScope (kfinal: kprev: {
        plasma-workspace = kprev.plasma-workspace.overrideAttrs (oldAttrs: {
          requiredSystemFeatures = [ "big-parallel" ];
          patches = (oldAttrs.patches or [ ]) ++ [
            (final.fetchpatch {
              name = "plasma-workspace-bugfix.patch";
              # Appending .patch to a GitLab/GitHub commit URL yields the raw diff
              url = "https://invent.kde.org/plasma/plasma-workspace/-/commit/5cf0241797e9df5ce28b4330a93a43cf045f6ae8.patch";
              hash = "sha256-LCpXxkIo7ExZKycDm663/JfZnsOUCpnm4fGpQX7v4yw=";
            })
          ];
        });
      });
    })
  ] else [];

  # Enable the KDE Plasma Desktop Environment.
  # TODO: Autostarts
  # TODO: timeout
  services.displayManager.sddm.enable = true;
  services.desktopManager.plasma6.enable = true;
  services.displayManager.defaultSession = "plasmax11";

  environment.systemPackages = with pkgs; [
    kdePackages.ksshaskpass
    kdePackages.kconfig
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
