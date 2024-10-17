{ config, pkgs, ... }:
{

  environment.systemPackages = with pkgs; [
    rWrapper.override{ packages = with rPackages; [
      rix
      tidyverse
    ]; };
    writeScriptBin "rixi" ''
      ${pkgs.R}/bin/Rscript --cmd "
      # This will create two files: .Rprofile and default.nix
      rix::rix(
        r_ver = \"4.4.1\",
        r_pkgs = c($(cat requirements.txt) "dplyr", "ggplot2"),
        system_pkgs = NULL,
        git_pkgs = NULL,
        ide = \"other\",
        project_path = \".\",
        overwrite = TRUE,
        print = TRUE
      )
      "
  ''
  ];

}
