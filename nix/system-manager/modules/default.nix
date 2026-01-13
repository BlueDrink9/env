{ config, lib, pkgs, ... }:

{
  imports = [
    # /etc/local-packages.nix
      /etc/system-manager.nix
    # ./vr.nix
    ./hardware/tobii5.nix
  ];
  config = {
    nixpkgs.hostPlatform = "x86_64-linux";
    system-manager.allowAnyDistro = true;

    # environment.sessionVariables = rec {
    # };

    environment = {
      systemPackages = with pkgs; [
      ];
      etc = {
        # "foo.conf".text = ''
        #   launch_the_rockets = true
        # '';
      };
    };

    systemd.services = {
    };

    # system.activationScripts.udev.text = ''
    #   #!${pkgs.runtimeShell}
    #   PATH=$PATH:${lib.makeBinPath [ pkgs.systemd ]}
    #   udevadm control --reload-rules
    #   udevadm trigger
    # '';

  };
}
