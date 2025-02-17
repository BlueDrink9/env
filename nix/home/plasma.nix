{ config, pkgs, lib, qttools, ... }:
let
  plasma6-applets-window-title = import ./plasma6-applets-window-title.nix { inherit pkgs lib; };
  # sweet-theme-kde = import ./sweet-theme-kde.nix { inherit pkgs lib; };
dotfilesDir = builtins.getEnv "DOTFILES_DIR";
in
  {
  home.packages = with pkgs; [
    plasma6-applets-window-title
    # sweet-theme-kde
    sweet-nova
    candy-icons
    kdePackages.qttools
  ];

  imports = [
    ./plasma-fakwin.nix
  ];

  programs.plasma = {
    enable = true;

    fonts = {
      general = {
        family = "SF Pro";
        pointSize = 10;
      };
      fixedWidth = {
        family = "SauceCodePro NF";
        pointSize = 10;
      };
    };

    powerdevil = {
      AC = {
        powerButtonAction = "shutDown";
        whenLaptopLidClosed = "doNothing";
        autoSuspend = {
          action = "sleep";
          idleTimeout = 120*60;
        };
        dimDisplay = {
          enable = true;
          idleTimeout = 2*60;
        };
        turnOffDisplay = {
          idleTimeout = 5*60;
          idleTimeoutWhenLocked = "immediately";
        };
      };
      battery = {
        inhibitLidActionWhenExternalMonitorConnected = true;
        whenLaptopLidClosed = "sleep";
        powerButtonAction = "shutDown";
        whenSleepingEnter = "standbyThenHibernate";
        displayBrightness = 70;
        dimDisplay = {
          enable = true;
          idleTimeout = 1*60;
        };
        turnOffDisplay = {
          idleTimeout = 3*60;
          idleTimeoutWhenLocked = "immediately";
        };
      };
      lowBattery = {
        whenLaptopLidClosed = "hibernate";
      };
    };

    kscreenlocker = {
      lockOnResume = true;
      timeout = 60;
      autoLock = false;
      passwordRequiredDelay = 5;
      appearance = {
        showMediaControls = true;
        alwaysShowClock = true;
      };
    };

    input.keyboard.layouts = [
      {
        layout = "us";
        variant = "colemak";
        displayName = "co";
      }
      {
        layout = "nz";
        displayName = "qw";
      }
    ];

    configFile.kxkbrc = {
      Layout = {
        DisplayNames = "co,qw";
        LayoutList = "us,nz";
        Options = "grp:win_space_toggle,grp_led:caps";
        ResetOldOptions = true;
        Use = true;
        VariantList = "colemak,";
      };
    };

    configFile.kdeglobals.General = {
      BrowserApplication = "firefox.desktop";
      TerminalApplication = "kitty";
    };

    spectacle.shortcuts.captureRectangularRegion = "";

    # workspace.theme = "sweet";
    # workspace.iconTheme = "candy";
    # workspace.windowDecorations = "mojave";
    # workspace.windowDecorations = "sweet";

    panels = [
      {
        location = "bottom";
        height=44;
        lengthMode = "fill";
        screen = "all";
        widgets = [
          {
            kickoff = {
              sortAlphabetically = true;
            };
          }
          "org.kde.plasma.marginsseparator"
          {
            name = "org.kde.plasma.pager";
            config = {
              General = {
                displayedText = "name";
                showWindowIcons = true;
                wrapPage = true;
                showOnlyCurrentScreen = true;
              };
            };
          }
          "org.kde.plasma.panelspacer"
          {
            name = "org.kde.windowtitle";
            config = {
              General = {
                boldFont = false;
                capitalFont = false;
                containmentType = "Plasma";
                lengthLastMargin = 15;
                maximumLength = 1499;
                lengthPolicy = "Maximum";
                perScreenActive = true;
                spacing = 0;
                style = 3;
              };
              Appearance = {
                # 'middle'
                elidePos = 2;
                # Icon fills thickness of panel
                fillThickness = true;
              };
            };
          }
          "org.kde.plasma.panelspacer"
          {
            systemTray.items = {
              shown = [
                # "org.kde.plasma.battery"
                # "org.kde.plasma.bluetooth"
                # "org.kde.plasma.volume"
              ];
              hidden = [
                # "org.kde.plasma.networkmanagement"
              ];
            };
          }
          "org.kde.plasma.digitalclock"
        ];
        # hiding = "autohide";
      }
    ];

    configFile.kiorc = {
      Confirmations.ConfirmDelete = true;
      Confirmations.ConfirmEmptyTrash = true;
      Confirmations.ConfirmTrash = false;
      "Executable scripts"."behaviourOnLaunch" = "execute";
    };

    configFile.plasma-localerc.Formats.LANG = "en_NZ.UTF-8";

    configFile.dolphinrc = {
      General = {
        BrowseThroughArchives = true;
        GlobalViewProps = false;
        ShowCopyMoveMenu = true;
        ShowToolTips = true;
        UseTabForSwitchingSplitView = true;
        ShowFullPathInTitlebar=true;
      };
      CompactMode.PreviewSize = 32;
      ContentDisplay.DirectorySizeCount = false;
      ContentDisplay.RecursiveDirectorySizeLimit = 9;
      # IconsMode.IconSize = 80;
      # IconsMode.MaximumTextLines = 5;
      # IconsMode.PreviewSize = 80;
      # IconsMode.TextWidthIndex = 0;
      "Notification Messages"."ConfirmDelete" = true;
      "Open-with settings"."CompletionMode" = 1;
      "Open-with settings"."History" = "ark,/bin/python,thunde,vim,inks,sh";
      "PreviewSettings"."Plugins" = "audiothumbnail,comicbookthumbnail,djvuthumbnail,ebookthumbnail,exrthumbnail,directorythumbnail,fontthumbnail,imagethumbnail,jpegthumbnail,kraorathumbnail,windowsexethumbnail,windowsimagethumbnail,opendocumentthumbnail,svgthumbnail,textthumbnail";
    };

    configFile.kdeglobals."KFileDialog Settings" = {
      "Breadcrumb Navigation" = true;
      "Decoration position" = 0;
      "LocationCombo Completionmode" = 5;
      "PathCombo Completionmode" = 5;
      "Show Bookmarks" = false;
      "Show Full Path" = false;
      "Show Inline Previews" = true;
      "Show Preview" = false;
      "Show Speedbar" = true;
      "Show hidden files" = false;
      "Sort by" = "Date";
      "Sort directories first" = true;
      "Sort hidden files last" = false;
      "Sort reversed" = true;
      "Speedbar Width" = 138;
      "View Style" = "Detail";
    };


    shortcuts = {
      ksmserver = {
        "Lock Session" = ["Screensaver" "Ctrl+Alt+L"];
      };
      "KDE Keyboard Layout Switcher"."Switch keyboard layout to English (Colemak)" = [ ];
      "KDE Keyboard Layout Switcher"."Switch keyboard layout to English (US)" = [ ];
      "KDE Keyboard Layout Switcher"."Switch to Last-Used Keyboard Layout" = [ ];
      "KDE Keyboard Layout Switcher"."Switch to Next Keyboard Layout" = [ ];
      "kaccess"."Toggle Screen Reader On and Off" = "none,Meta+Alt+S,Toggle Screen Reader On and Off";
      "kcm_touchpad"."Disable Touchpad" = "none,Touchpad Off,Disable Touchpad";
      "kcm_touchpad"."Enable Touchpad" = "none,Touchpad On,Enable Touchpad";
      "kcm_touchpad"."Toggle Touchpad" = "none,Touchpad Toggle,Toggle Touchpad";
      kmix = {
        "decrease_microphone_volume" = "none,Microphone Volume Down,Decrease Microphone Volume";
        "decrease_volume" = "none,Volume Down,Decrease Volume";
        "decrease_volume_small" = "none,Shift+Volume Down,Decrease Volume by 1%";
        "increase_microphone_volume" = "none,Microphone Volume Up,Increase Microphone Volume";
        "increase_volume" = "none,Volume Up,Increase Volume";
        "increase_volume_small" = "none,Shift+Volume Up,Increase Volume by 1%";
        "mic_mute" = ["none,Microphone Mute" "Meta+Volume Mute,Mute Microphone"];
        "mute" = "none,Volume Mute,Mute";
      };
      ksmserver = {
        "Halt Without Confirmation" = "none,,Shut Down Without Confirmation";
        "Log Out" = "none,Ctrl+Alt+Del,Show Logout Prompt";
        "Log Out Without Confirmation" = "none,,Log Out Without Confirmation";
        "LogOut" = "none,,Log Out";
        "Reboot" = "none,,Reboot";
        "Reboot Without Confirmation" = "none,,Reboot Without Confirmation";
        "Shut Down" = "none,,Shut Down";
      };
      "ktorrent"."queue_suspend" = "Alt+Shift+P";
      "ktorrent"."show_kt" = "Alt+Shift+T";
      mediacontrol = {
        "mediavolumedown" = "Volume Down,,Media volume down";
        "mediavolumeup" = "Volume Up,,Media volume up";
        "nextmedia" = "Media Next";
        "pausemedia" = "Media Pause";
        "playmedia" = "none,,Play media playback";
        "playpausemedia" = "Media Play";
        "previousmedia" = "Media Previous";
        "stopmedia" = "none,Media Stop,Stop media playback";
      };
      org_kde_powerdevil = {
        "Decrease Keyboard Brightness" = "Keyboard Brightness Down";
        "Decrease Screen Brightness" = "none,Monitor Brightness Down,Decrease Screen Brightness";
        "Decrease Screen Brightness Small" = "none,Shift+Monitor Brightness Down,Decrease Screen Brightness by 1%";
        "Hibernate" = "none,Hibernate,Hibernate";
        "Increase Keyboard Brightness" = "Keyboard Brightness Up";
        "Increase Screen Brightness" = "none,Monitor Brightness Up,Increase Screen Brightness";
        "Increase Screen Brightness Small" = "none,Shift+Monitor Brightness Up,Increase Screen Brightness by 1%";
        "PowerDown" = "none,Power Down,Power Down";
        "PowerOff" = "none,Power Off,Power Off";
        "Sleep" = "Sleep";
        "Toggle Keyboard Backlight" = "Keyboard Light On/Off";
        "Turn Off Screen" = [ ];
        "powerProfile" = ["none,Battery" "Meta+B,Switch Power Profile"];
      };
      plasmashell = {
        "activate application launcher" = ["Meta" "Alt+F1,Meta" "Alt+F1,Activate Application Launcher"];
        "activate task manager entry 1" = "none,Meta+1,Activate Task Manager Entry 1";
        "activate task manager entry 10" = "none,Meta+0,Activate Task Manager Entry 10";
        "activate task manager entry 2" = "none,Meta+2,Activate Task Manager Entry 2";
        "activate task manager entry 3" = "none,Meta+3,Activate Task Manager Entry 3";
        "activate task manager entry 4" = "none,Meta+4,Activate Task Manager Entry 4";
        "activate task manager entry 5" = "none,Meta+5,Activate Task Manager Entry 5";
        "activate task manager entry 6" = "none,Meta+6,Activate Task Manager Entry 6";
        "activate task manager entry 7" = "none,Meta+7,Activate Task Manager Entry 7";
        "activate task manager entry 8" = "none,Meta+8,Activate Task Manager Entry 8";
        "activate task manager entry 9" = "none,Meta+9,Activate Task Manager Entry 9";
        "clear-history" = "none,,Clear Clipboard History";
        "clipboard_action" = "none,Meta+Ctrl+X,Automatic Action Popup Menu";
        "cycle-panels" = "none,Meta+Alt+P,Move keyboard focus between panels";
        "cycleNextAction" = "none,,Next History Item";
        "cyclePrevAction" = "none,,Previous History Item";
        "manage activities" = "none,Meta+Q,Show Activity Switcher";
        "next activity" = [ ];
        "previous activity" = [ ];
        "repeat_action" = "none,Meta+Ctrl+R,Manually Invoke Action on Current Clipboard";
        "show dashboard" = "none,Ctrl+F12,Show Desktop";
        "show-barcode" = "none,,Show Barcode…";
        "show-on-mouse-pos" = "none,Meta+V,Show Clipboard Items at Mouse Position";
        "stop current activity" = "none,Meta+S,Stop Current Activity";
        "switch to next activity" = "none,,Switch to Next Activity";
        "switch to previous activity" = "none,,Switch to Previous Activity";
        "toggle do not disturb" = "none,,Toggle do not disturb";
      };
      "services/org.kde.dolphin.desktop"."_launch" = [ ];
      "services/org.kde.spectacle.desktop"."RecordRegion" = "Meta+Shift+R";
      "services/org.kde.spectacle.desktop"."_launch" = "Print";
    };

    configFile = {
      # "baloofilerc"."General"."dbVersion" = 2;
      # "baloofilerc"."General"."exclude filters" = "*~,*.part,*.o,*.la,*.lo,*.loT,*.moc,moc_*.cpp,qrc_*.cpp,ui_*.h,cmake_install.cmake,CMakeCache.txt,CTestTestfile.cmake,libtool,config.status,confdefs.h,autom4te,conftest,confstat,Makefile.am,*.gcode,.ninja_deps,.ninja_log,build.ninja,*.csproj,*.m4,*.rej,*.gmo,*.pc,*.omf,*.aux,*.tmp,*.po,*.vm*,*.nvram,*.rcore,*.swp,*.swap,lzo,litmain.sh,*.orig,.histfile.*,.xsession-errors*,*.map,*.so,*.a,*.db,*.qrc,*.ini,*.init,*.img,*.vdi,*.vbox*,vbox.log,*.qcow2,*.vmdk,*.vhd,*.vhdx,*.sql,*.sql.gz,*.ytdl,*.class,*.pyc,*.pyo,*.elc,*.qmlc,*.jsc,*.fastq,*.fq,*.gb,*.fasta,*.fna,*.gbff,*.faa,po,CVS,.svn,.git,_darcs,.bzr,.hg,CMakeFiles,CMakeTmp,CMakeTmpQmake,.moc,.obj,.pch,.uic,.npm,.yarn,.yarn-cache,__pycache__,node_modules,node_packages,nbproject,core-dumps,lost+found";
      # "baloofilerc"."General"."exclude filters version" = 8;
      "kcminputrc"."Mouse"."X11LibInputXAccelProfileFlat" = true;
    };


    dataFile = {
      "dolphin/view_properties/global/.directory"."Settings"."HiddenFilesShown" = true;
    };
  };

  # Replace kwin with bspwm
  xdg.configFile."plasma-workspace/env/bspwm.sh" = {
    text = ''
        #!/bin/sh
         export KDEWM=bspwm
    '';
    force = true;
  };

  # Change wallpaper every day
  systemd.user.timers."plasma-earthporn-wallpapers" = {
    Unit.Description = "change wallpaper every day";
    Timer = {
      Unit = "plasma-earthporn-wallpapers";
      partOf = [ "plasma-earthporn-wallpapers.service" ];
      OnCalendar = "08:00";
      Persistent = true;
    };
    Install.WantedBy = [ "timers.target" ];
  };
  systemd.user.services."plasma-earthporn-wallpapers" = {
    Unit = {
      Description = "change wallpaper every day";
      After = [ "network.target" "network-online.target" ];
    };
    Service = {
      Type = "oneshot";
      # ExecPreStart = "/usr/bin/env sleep 120";
      ExecStart = ../../shell/scripts/plasma_earthporn_wallpapers.sh;
    };
    Install.WantedBy = [ "default.target" ];
  };

  xdg.configFile."systemd/user/plasma-bspwm.service" = {
    text = builtins.readFile "${dotfilesDir}/windowManagers/bspwm/plasma_wm_replace/plasma-bspwm.service";
    executable = false;
    force=true;
  };
  # Mask the kwin service
  # xdg.configFile."systemd/user/plasma-kwin.service".source = "/dev/null";
  # systemd.user.services.plasma-kwin_x11.enabled = false;
  systemd.user.services.plasma-workspace-x11.target.wants = [ "plasma-bspwm.service" ];
  qt.kde.settings = {
    startkderc.general.systemdBoot = false;
  };
  # home.activation = {
  #   configureKDE = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
  #   run systemctl --user mask plasma-kwin_x11.service
  #   run systemctl --user enable plasma-bspwm.service
  #   '';
  # };

  # Restart plasmashell
  # Not much point actually, since the panel script doesn't run until autostarting and is tricky to run from within a nix environment.
  # ${pkgs.plasma-manager}/run_all.sh
  # home.activation.restartPlasmaShell =
  #   config.lib.dag.entryAfter ["reloadSystemd"]
  #   ''${pkgs.systemd}/bin/systemctl restart --user plasma-plasmashell && \
  #     ${pkgs.systemd}/bin/systemctl enable --user plasma-plasmashell
  #     ${pkgs.kdePackages.qttools}/bin/qdbus org.kde.plasmashell /PlasmaShell org.kde.PlasmaShell.refreshCurrentShell
  #   '';

}
