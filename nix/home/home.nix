let
  dotfilesDir = builtins.getEnv "DOTFILES_DIR";
  nixDir = "${dotfilesDir}/system/nix";
in { config, pkgs, lib, ... }:

let
  unstable = import <nixpkgs-unstable> {};
  unstableTarball =
    fetchTarball
      https://github.com/NixOS/nixpkgs/archive/nixos-unstable.tar.gz;
in

  {
  imports = [
    <plasma-manager/modules>
    # ./plasma.nix
    ./talon.nix
    # ./mime_apps.nix
  ];


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

  home.stateVersion = "24.05";

  home.packages = with pkgs; [
  ];

  xdg.enable = true;

  # Home Manager is pretty good at managing dotfiles. The primary way to manage
  # plain files is through 'home.file'.
  home.file = {
    # # Building this configuration will create a copy of 'dotfiles/screenrc' in
    # # the Nix store. Activating the configuration will then make '~/.screenrc' a
    # # symlink to the Nix store copy.
    # ".screenrc".source = dotfiles/screenrc;

    # # You can also set the file content immediately.
    # ".gradle/gradle.properties".text = ''
    #   org.gradle.console=verbose
    #   org.gradle.daemon.idletimeout=3600000
    # '';
  };

  # Home Manager can also manage your environment variables through
  # 'home.sessionVariables'. These will be explicitly sourced when using a
  # shell provided by Home Manager. If you don't want to manage your shell
  # through Home Manager then you have to manually source 'hm-session-vars.sh'
  # located at either
  #
  #  ~/.nix-profile/etc/profile.d/hm-session-vars.sh
  #
  # or
  #
  #  ~/.local/state/nix/profiles/profile/etc/profile.d/hm-session-vars.sh
  #
  # or
  #
  #  /etc/profiles/per-user/william/etc/profile.d/hm-session-vars.sh
  #
  fonts.fontconfig.defaultFonts = {
    sansSerif = [
      "SF Pro"
    ];
    monospace = [
      "SauceCodePro NF"
    ];
  };

  home.keyboard = {
    layout = "us,us";
    variant = "colemak,";
    options = [
      "grp:win_space_toggle"
      "grp_led:caps"
    ];
  };

  home.sessionPath = [
    "$HOME/.local/bin"
  ];

  # Perform garbage collection weekly to maintain low disk usage
  nix.gc = {
    automatic = true;
    # dates = "weekly";
    options = "--delete-older-than 2w";
  };

  home.sessionVariables = {
    EDITOR = "nvim";
  };

  # xsession.windowManager.bspwm.enable = true;
  # xsession.windowManager.bspwm.extraConfig = ''
  # . "${dotfilesDir}/windowManagers/bspwm/bspwmrc"
  # if [ -f local_bspwmrc ]; then
  #   . local_bspwmrc
  # fi
  # '';

  home.preferXdgDirectories = true;


  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  services.redshift = {
    enable = true;
    tray = true;
    latitude = -41.28664;
    longitude = 174.77557;
    temperature.day = 6500;
    temperature.night = 2500;
    settings.redshift.brightness-night = 0.7;
  };

  services.ssh-agent.enable = true;
  services.syncthing = {
    # Default to true, but on NixOS systems this should be overridden to false.
    enable = lib.mkDefault true;
    tray.enable = true;
  };

  programs.joplin-desktop = {
    enable = true;
    sync.interval = "5m";
    sync.target = "dropbox";
    extraConfig = {
      "editor.codeView"= true;
        "richTextBannerDismissed"= true;
        "locale"= "en_GB";
        "theme"= 1;
        "themeAutoDetect"= true;
        "preferredDarkTheme"= 5;
        "notes.sortOrder.field"= "title";
        "editor.autoMatchingBraces"= false;
        "markdown.plugin.softbreaks"= false;
        "markdown.plugin.typographer"= false;
        "markdown.plugin.sub"= true;
        "markdown.plugin.sup"= true;
        "showTrayIcon"= true;
        "startMinimized"= false;
        "style.editor.fontFamily"= "SF Pro";
      "style.editor.monospaceFontFamily"= "SauceCodePro NF";
      "ui.layout"= {
        "key"= "root";
        "children"= [
          {
            "key"= "tempContainer-LqrvxPVCrSUHMeVk2E5evZ";
            "children"= [
              {
                "key"= "plugin-view-joplin.plugin.benji.favorites-favorites.panel";
                "context"= {
                  "pluginId"= "joplin.plugin.benji.favorites";
                };
                "visible"= true;
                "height"= 46;
              }
              {
                "key"= "sideBar";
                "visible"= true;
                "height"= 414;
              }
              {
                "key"= "noteList";
                "visible"= true;
                "height"= 433;
              }
              {
                "key"= "plugin-view-outline-outline.panel";
                "context"= {
                  "pluginId"= "outline";
                };
                "visible"= true;
              }
            ];
            "visible"= true;
            "width"= 352;
          }
          {
            "key"= "tempContainer-4LVBFLXPHawEEQXm1pawGY";
            "children"= [
              {
                "key"= "plugin-view-joplin.plugin.note.tabs-note.tabs.panel";
                "context"= {
                  "pluginId"= "joplin.plugin.note.tabs";
                };
                "visible"= true;
                "height"= 40;
              }
              {
                "key"= "editor";
                "visible"= true;
              }
              {
                "key"= "plugin-view-ylc395.noteLinkSystem-panel";
                "context"= {
                  "pluginId"= "ylc395.noteLinkSystem";
                };
                "visible"= false;
              }
            ];
            "visible"= true;
          }
        ];
        "visible"= true;
      };
      "clipperServer.autoStart"= true;
      "noteVisiblePanes"= [
        "viewer"
      ];
      "editor"= "${pkgs.neovide}/bin/neovide";
      "editor.keyboardMode"= "vim";
      "revisionService.ttlDays"= 480;
      "spellChecker.language"= "en-GB";
      "spellChecker.languages"= [
        "en-GB"
      ];
      "windowContentZoomFactor"= 100;
    };
  };

  programs.okular.general.openFileInTabs = true;
}
