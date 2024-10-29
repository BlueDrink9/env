
{ config, pkgs, ... }:

{
  programs.mepo.enable = true;
  programs.steam = {
    enable = true;
    remotePlay.openFirewall = true;
    localNetworkGameTransfers.openFirewall = true;
  };

  my.pkgs = with pkgs; [
    heroic
    cider
  ];

}
