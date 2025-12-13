{ lib, config, pkgs, ... }:
# Scripts that need to be run as root, and it's ok to do so for any user.
let
  resetTobiiHub = pkgs.writeScriptBin "reset-tobii-hub" (builtins.readFile ../shell/scripts/reset-tobii-hub.py);
in
{
  environment.systemPackages = with pkgs; [
    resetTobiiHub
  ];

  security.sudo.extraRules = [
    {
      users = [ "ALL" ];
      commands = [
        {
          command = "/run/current-system/sw/bin/reset-tobii-hub";
          options = [ "NOPASSWD" ];
        }
      ];
    }
  ];

}
