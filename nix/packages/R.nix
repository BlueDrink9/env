{ config, pkgs, ... }:
{

  environment.systemPackages = with pkgs;
    let R = rWrapper.override{ packages = with rPackages; [
      # rix
      (pkgs.rPackages.buildRPackage {
        name = "rix";
        src = pkgs.fetchgit {
          url = "https://github.com/ropensci/rix/";
          rev = "287e8bd5d41649247747a499e459ef33cc7c76e0";
          sha256 = "sha256-TaSzPijuk2EdmXfghd89mvNHpVgNqfYJsydt5/BPmGw=";
        };
        propagatedBuildInputs = builtins.attrValues {
          inherit (pkgs.rPackages)
          codetools
          curl
          jsonlite
          sys;
        };
      })
      tidyverse
      languageserver
    ]; };
      in [
      R
      air-formatter
      python312Packages.radian
      (writeScriptBin "rixi" ''
      # This will create two files: .Rprofile and default.nix
      ${R}/bin/R -e '
        requirements <- c(
          gsub(
            "\n", ", ",
            system2("cat", args=c("requirements.txt", "||", "echo"), stdout = TRUE, stderr = FALSE),
          ),
        )
        rix::rix(
          r_ver = "4.3.3",
          r_pkgs = requirements,
          system_pkgs = NULL,
          git_pkgs = NULL,
          ide = "other",
          project_path = ".",
          overwrite = TRUE,
          print = TRUE
        )
      '
      '')
      ];

}
