{ pkgs, ... }:
{
  # Allow unfree packages
  nixpkgs.config.allowUnfree = true;

  programs.zsh.enable = true;
  programs.git.enable = true;
  programs.htop.enable = true;

  programs.kdeconnect.enable = true;

  programs.adb.enable = true;
  programs.appimage.enable = true;
  programs.direnv.enable = true;
  programs.direnv.direnvrcExtra = "echo Loaded direnv";


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

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    vim-full
    zsh
    neovim
    wget

    gcc
    libgcc
    gnumake
    cmake
    automake
    pkg-config

    python3
    bspwm
    sxhkd
    xclip
    xsel
    ripgrep
    fd
    kitty
    tmux
    unzip
    lsd
    bat
    zoxide
    syncthing
    btop
    ranger
    rsync
    mosh
    jq
    cheat
    openssh
    rlwrap
    curl
    ctags
    universal-ctags
    ncurses
    bat-extras.batgrep
    broot
    duf
    mcfly
    thefuck
    tldr
    wget
    openvpn
    topgrade
    playerctl
    workrave
    gh
    lazygit

    shellcheck
    pyright


    emacs
    vscodium
    thunderbird
    birdtray
    etesync-dav
    keepassxc
    pandoc
    typst
    copyq
    inkscape
    gimp
    zotero
    joplin
    signal-desktop
    neovide
    brave
    pcloud
    megatools
    f3d
    libreoffice-qt-fresh

    hunspell
    hunspellDicts.en_AU

  ];

}
