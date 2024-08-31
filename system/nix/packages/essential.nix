{ pkgs, ... }:
{

  # Allow unfree packages
  nixpkgs.config.allowUnfree = true;

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
    neovim
    wget
    curl
    nano  # always nice to have a backup
    ripgrep
    fd
    tmux
    gcc
    python3
    unzip
    lsd
    bat
    zoxide
    rsync
    jq
    rlwrap
    starship
    ranger
    openssh

  ];

}
