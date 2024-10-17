
{ config, pkgs, ... }:

{
  programs.mepo.enable = true;
  programs.steam = {
    enable = true;
    remotePlay.openFirewall = true;
    localNetworkGameTransfers.openFirewall = true;
  };

  environment.systemPackages = with pkgs; [
    heroic
    cider
  ];

}
