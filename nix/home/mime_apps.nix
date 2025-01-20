{ config, pkgs, lib, ... }:
{
  xdg.mimeApps = {
    enable = true;
    defaultApplications = {
      "text/plain" = [
        "neovide.desktop"
        "nvim.desktop"
        "gvim.desktop"
        "vim.desktop"
      ];
      "text/markdown" = [
        "neovide.desktop"
        "nvim.desktop"
        "gvim.desktop"
        "vim.desktop"
      ];
      "text/html" = [
        "firefox.desktop"
        "neovide.desktop"
        "nvim.desktop"
        "gvim.desktop"
        "vim.desktop"
      ];
      "application/json" = "neovide.desktop";
      "application/pdf" = "okular.desktop";
      "image/png" = [
        "gwenview.desktop"
        "gimp.desktop"
      ];
      "image/svg+xml" = [
        "firefox.desktop"
        "inkscape.desktop"
        "gwenview.desktop"
      ];
      "x-scheme-handler/http" = "firefox.desktop";
      "x-scheme-handler/https" = "firefox.desktop";
    };
  };
}
