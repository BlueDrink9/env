
{ config, lib, pkgs, ... }:

{
  config = {
    environment = {
      etc = {
        "udev/rules.d/10-talon.rules".text = (builtins.readFile ./tobii5-udev.rules);
      };
    };

    # "Activation" script to reload udev rules, in case sys-mgr has changed them.
    systemd.services.reload-udev-rules = {
      description = "Reload udev rules";
      enable = true;
      # Start when sysmgr acviates
      wantedBy = [ "system-manager.target" ];
      after = [ "systemd-udevd.service" ];
      serviceConfig = {
        Type = "oneshot";
        RemainAfterExit = true;
      };
      script = ''
        ${pkgs.systemd}/bin/udevadm control --reload-rules
      '';
    };

  };
}
