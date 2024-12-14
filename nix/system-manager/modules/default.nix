{ config, lib, pkgs, ... }:

{
  config = {
    nixpkgs.hostPlatform = "x86_64-linux";
    system-manager.allowAnyDistro = true;

    environment = {
      etc = {
        # "foo.conf".text = ''
        #   launch_the_rockets = true
        # '';
      };
      systemPackages = [
      ];
    };

    systemd.services = {
    };
  };
}
