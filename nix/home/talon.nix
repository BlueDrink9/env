{ lib, config, pkgs, ... }:
{

  home.packages = [
    (builtins.getFlake "github:nix-community/talon-nix").packages.${builtins.currentSystem}.default
  ];

  home.activation.talon = ''
    pushd /home/$USER/.talon/user/
    [ ! -d community ] && ${pkgs.git}/bin/git clone https://github.com/talonhub/community
    [ ! -d bluedrink9-talon ] && ${pkgs.git}/bin/git clone https://github.com/bluedrink9/bluedrink9-talon

    [ ! -d rango-talon ] && ${pkgs.git}/bin/git clone https://github.com/david-tejada/rango-talon
    [ ! -d talon-ai-tools ] && ${pkgs.git}/bin/git clone https://github.com/c-loftus/talon-ai-tools
    # [ ! -d talon_hud ] && ${pkgs.git}/bin/git clone https://github.com/chaosparrot/talon_hud
    [ ! -d cursorless-talon ] && ${pkgs.git}/bin/git clone https://github.com/cursorless-dev/cursorless-talon.git
    [ ! -d neovim-talon ] && ${pkgs.git}/bin/git clone https://github.com/hands-free-vim/neovim-talon.git
    [ ! -d talon-vim ] && ${pkgs.git}/bin/git clone https://github.com/fidgetingbits/talon-vim.git
    ../bin/pip install pynvim neovim
  '';

}
