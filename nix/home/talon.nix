{ lib, config, pkgs, ... }:
let
    talon_pkg = (builtins.getFlake "github:nix-community/talon-nix").packages.${builtins.currentSystem}.default;
    grm_pkg = (builtins.getFlake "github:hakoerber/git-repo-manager").packages.${builtins.currentSystem}.default;
  in
{

  home.packages = with pkgs; [
    talon_pkg
      grm_pkg
    # MS core fonts, needed for non-nixos linux?
    corefonts
    (python3.withPackages (python-pkgs: with python-pkgs; [
      pynvim
      neovim
    ]))
  ];
  fonts.fontconfig.enable = true;

  home.activation.talon = ''
    ${grm_pkg}/bin/grm repos sync config --config ${../../talon/talon_plugins.yml}
  '';

}
