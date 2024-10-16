{ config, pkgs, ... }:
let
  unstableTarball =
    fetchTarball
      https://github.com/NixOS/nixpkgs/archive/nixos-unstable.tar.gz;
in
{

  nixpkgs.config = {
    allowUnfree = true;
    # Create an alias for the unstable channel
    packageOverrides = pkgs: {
      # pass the nixpkgs config to the unstable alias to ensure `allowUnfree = true;` is propagated:
      unstable = import unstableTarball {
        config = config.nixpkgs.config;
      };
    };
  };


  programs.zsh.enable = true;
  programs.zsh.enableBashCompletion = true;
  programs.git.enable = true;
  programs.htop.enable = true;
  programs.neovim.defaultEditor = true;

  # programs.java.enable = true;
  # programs.hyperland.enable = true;
  # add hdrop to replace tdrop on hyperland

  # Essential packages are defined here, I'll basically always want these.
  # Rest are defined in imports
  environment.systemPackages = with pkgs; [
    vim-full
    bash
    zsh
    unstable.neovim
    wget
    curl
    nano  # always nice to have a backup
    ripgrep
    fd
    tmux
    gcc
    python3
    unzip
    zoxide
    rsync
    jq
    rlwrap
    starship
    ranger
    openssh
    pstree
    ncdu
  ];

}
