{ config, lib, pkgs, ... }:

# Fedora
{
    services.howdy.enable = true;
    environment.systemPackages = with pkgs; [
        howdy
    ];
}
