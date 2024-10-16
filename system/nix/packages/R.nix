{ config, pkgs, ... }:
{

  environment.systemPackages = with pkgs; [
    rWrapper.override{ packages = with rPackages; [
      rix
      tidyverse
    ]; };

}
