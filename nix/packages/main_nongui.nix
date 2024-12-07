{ config, pkgs, ... }:

{

  programs.ydotool.enable = true;
  programs.kdeconnect.enable = true;
  programs.system-config-printer.enable = true;
  programs.command-not-found.enable = true;
  programs.adb.enable = true;
  programs.appimage = {
    enable = true;
    binfmt = true;
  };

  # Faster, more automatic and safer direnvs. Run `lorri init` in project dirs.
  services.lorri.enable = true;
  programs.direnv = {
    enable = true;
    direnvrcExtra = "echo Loaded direnv";
  };


  services.blueman.enable = true;

  environment.systemPackages = with pkgs; [
    # Nix bash can cause issues with non-nix packages on non-nixOS
    # systems, which is why it isn't in essential. Still have modern
    # shell in essential via zsh.
    bash
    universal-ctags
    ncurses
    bat-extras.batgrep
    cheat
    navi
    tldr
    broot
    duf
    mcfly
    pay-respects
    bluetuith
    sqlite
    vivid
    bat
    eza
    python3

    veracrypt
    syncthing
    topgrade
    # Easily manage and update project dependencies
    niv
    # Github cli, for easy auth. gh auth && gh auth setup-git
    gh
    git-credential-manager
    lazygit
    shellcheck
    pyright
    nil # Nix lsp
    # Nix disk usage
    # nix-du -s=500MB > store.dot; zgrviewer store.dot
    nix-du zgrviewer
    chatblade
    gnumake
    openssh

# so many programs rely on this for setup/install, so we'll put it in main.
    gcc
    cmake

    # Easiest to just let Mason install (and hence trigger configuration for) some packages.
    # Mason needs these installers though.
    cargo

  ];

}
