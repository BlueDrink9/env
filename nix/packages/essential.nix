{ config, pkgs, lib, inputs ? {}, ... }:

{

  programs.zsh.enable = true;
  programs.zsh.enableBashCompletion = true;
  programs.bash.completion.enable = true;
  programs.git.enable = true;
  programs.htop.enable = true;
  programs.neovim = {
    defaultEditor = true;
    # viAlias = true;
    # vimAlias = true;
    withPython3 = true;
  };

  # programs.java.enable = true;
  # programs.hyperland.enable = true;
  # add hdrop to replace tdrop on hyperland

  # Essential packages are defined here, I'll basically always want these.
  # Rest are defined in imports
  environment.systemPackages = with pkgs; [
      # These are a few packages we always want latest off, and it pays to have installation redundencies.
      unstable.neovim
      unstable.git
      unstable.tmux
      unstable.zsh
      vim-full
      wget
      curl
      nano  # always nice to have a backup
      ripgrep
      fd
      unstable.fzf
      unzip
      zoxide
      rsync
      jq
      rlwrap
      starship
      ranger
      pstree
      ncdu
      nix-search-cli
      nix-output-monitor

      jujutsu
      lazyjj
    ];

}
