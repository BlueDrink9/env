{ pkgs, ... }:
{

  imports =
  [
    # (import (builtins.getFlake "github:xremap/nix-flake").nixosModules.default { system = "x86_64-linux"; })
    ./main_nongui.nix
    ./gui.nix
    ./plasma.nix
  ];

  # Allow unfree packages
  nixpkgs.config.allowUnfree = true;

  programs.zsh.enable = true;
  programs.zsh.enableBashCompletion = true;
  programs.git.enable = true;
  programs.htop.enable = true;
  programs.neovim.defaultEditor = true;
  programs.ydotool.enable = true;

  programs.kdeconnect.enable = true;
  programs.partition-manager.enable = true;

  programs.mepo.enable = true;
  programs.steam = {
    enable = true;
    remotePlay.openFirewall = true;
    localNetworkGameTransfers.openFirewall = true;
  };

  programs.system-config-printer.enable = true;
  programs.adb.enable = true;
  programs.appimage.enable = true;
  programs.direnv.enable = true;
  programs.direnv.direnvrcExtra = "echo Loaded direnv";

  # programs.java.enable = true;
  # programs.hyperland.enable = true;

  programs.firefox = {
    enable = true;

    /* ---- POLICIES ---- */
    # Check about:policies#documentation for options.
    policies = {
      DisableTelemetry = true;
      DisableFirefoxStudies = true;
      EnableTrackingProtection = {
        Value= true;
        Locked = true;
        Cryptomining = true;
        Fingerprinting = true;
      };
      DisablePocket = true;
      OverrideFirstRunPage = "";
      OverridePostUpdatePage = "";
      # DontCheckDefaultBrowser = true;
      DisplayBookmarksToolbar = "newtab";
      DisplayMenuBar = "default-off"; # alternatives: "always", "never" or "default-on"
      SearchBar = "unified"; # alternative: "separate"

      /* ---- EXTENSIONS ---- */
      # Check about:support for extension/add-on ID strings.
      # Valid strings for installation_mode are "allowed", "blocked",
      # "force_installed" and "normal_installed".
      ExtensionSettings = with builtins;
        let extension = shortId: uuid: {
          name = uuid;
          value = {
            install_url = "https://addons.mozilla.org/en-US/firefox/downloads/latest/${shortId}/latest.xpi";
            installation_mode = "normal_installed";
          };
        };
        in listToAttrs [
            # "*".installation_mode = "blocked"; # blocks all addons except the ones specified below
            # uBlock Origin:
            (extension "ublock-origin" "uBlock0@raymondhill.net")
            # Privacy Badger:
            (extension "privacy-badger17" "jid1-MnnxcxisBPnSXQ@jetpack")
            # vimium:
            (extension "vimium-c" "vimium-c@gdh1995.cn")
          ];

      /* ---- PREFERENCES ---- */
      # Check about:config for options.
      Preferences = {
        # "browser.contentblocking.category" = { Value = "strict"; Status = "locked"; };
        "extensions.pocket.enabled" = false;
        # "extensions.screenshots.disabled" = lock-true;
        # "browser.topsites.contile.enabled" = lock-false;
        # "browser.formfill.enable" = lock-false;
        # "browser.search.suggest.enabled" = lock-false;
        # # "browser.search.suggest.enabled.private" = lock-false;
        # "browser.urlbar.suggest.searches" = lock-false;
        # "browser.urlbar.showSearchSuggestionsFirst" = lock-false;
        # "browser.newtabpage.activity-stream.feeds.section.topstories" = lock-false;
        # "browser.newtabpage.activity-stream.feeds.snippets" = lock-false;
        # "browser.newtabpage.activity-stream.section.highlights.includePocket" = lock-false;
        # "browser.newtabpage.activity-stream.section.highlights.includeBookmarks" = lock-false;
        # "browser.newtabpage.activity-stream.section.highlights.includeDownloads" = lock-false;
        # "browser.newtabpage.activity-stream.section.highlights.includeVisited" = lock-false;
        "browser.newtabpage.activity-stream.showSponsored" = false;
        "browser.newtabpage.activity-stream.system.showSponsored" = false;
        "browser.newtabpage.activity-stream.showSponsoredTopSites" = false;
        # disable all the annoying quick actions
        "browser.urlbar.quickactions.enabled" = false;
        "browser.urlbar.quickactions.showPrefs" = false;
        "browser.urlbar.shortcuts.quickactions" = false;
        "browser.urlbar.suggest.quickactions" = false;
        # Default search engine?
        "browser.urlbar.placeholderName" = "DuckDuckGo";
      };
    };
  };

  fonts.packages = with pkgs; [
    # source-code-pro
    (nerdfonts.override { fonts = [ "SourceCodePro" "Meslo" ]; })
    # menlo
    # san-franciso
  ];

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

    # xremap-flake.nixosModules.default

  ];

}
