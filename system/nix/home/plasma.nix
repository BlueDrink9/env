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
        # autoSuspend = {
        #   action = "sleep";
        #   idleTimeout = 120*60;
        # };
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

    configFile.kxkbrc = {
      Layout = {
        DisplayNames = ",";
        LayoutList = "us,us";
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
                lengthPolicy = "Maximum";
                perScreenActive = true;
                spacing = 0;
                style = 3;
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
      "Confirmations"."ConfirmDelete" = true;
      "Confirmations"."ConfirmEmptyTrash" = true;
      "Confirmations"."ConfirmTrash" = false;
      "Executable scripts"."behaviourOnLaunch" = "execute";
    };

    configFile.plasma-localerc.Formats.LANG = "en_NZ.UTF-8";

    configFile.dolphinrc = {
      "CompactMode"."PreviewSize" = 32;
      "ContentDisplay"."DirectorySizeCount" = false;
      "ContentDisplay"."RecursiveDirectorySizeLimit" = 9;
      "General"."BrowseThroughArchives" = true;
      "General"."GlobalViewProps" = false;
      "General"."ShowCopyMoveMenu" = true;
      "General"."ShowToolTips" = true;
      "General"."UseTabForSwitchingSplitView" = true;
      # "IconsMode"."IconSize" = 80;
      # "IconsMode"."MaximumTextLines" = 5;
      # "IconsMode"."PreviewSize" = 80;
      # "IconsMode"."TextWidthIndex" = 0;
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
      "kmix"."decrease_microphone_volume" = "none,Microphone Volume Down,Decrease Microphone Volume";
      "kmix"."decrease_volume" = "none,Volume Down,Decrease Volume";
      "kmix"."decrease_volume_small" = "none,Shift+Volume Down,Decrease Volume by 1%";
      "kmix"."increase_microphone_volume" = "none,Microphone Volume Up,Increase Microphone Volume";
      "kmix"."increase_volume" = "none,Volume Up,Increase Volume";
      "kmix"."increase_volume_small" = "none,Shift+Volume Up,Increase Volume by 1%";
      "kmix"."mic_mute" = ["none,Microphone Mute" "Meta+Volume Mute,Mute Microphone"];
      "kmix"."mute" = "none,Volume Mute,Mute";
      "ksmserver"."Halt Without Confirmation" = "none,,Shut Down Without Confirmation";
      "ksmserver"."Log Out" = "none,Ctrl+Alt+Del,Show Logout Prompt";
      "ksmserver"."Log Out Without Confirmation" = "none,,Log Out Without Confirmation";
      "ksmserver"."LogOut" = "none,,Log Out";
      "ksmserver"."Reboot" = "none,,Reboot";
      "ksmserver"."Reboot Without Confirmation" = "none,,Reboot Without Confirmation";
      "ksmserver"."Shut Down" = "none,,Shut Down";
      "ktorrent"."queue_suspend" = "Alt+Shift+P";
      "ktorrent"."show_kt" = "Alt+Shift+T";
      "mediacontrol"."mediavolumedown" = "Volume Down,,Media volume down";
      "mediacontrol"."mediavolumeup" = "Volume Up,,Media volume up";
      "mediacontrol"."nextmedia" = "Media Next";
      "mediacontrol"."pausemedia" = "Media Pause";
      "mediacontrol"."playmedia" = "none,,Play media playback";
      "mediacontrol"."playpausemedia" = "Media Play";
      "mediacontrol"."previousmedia" = "Media Previous";
      "mediacontrol"."stopmedia" = "none,Media Stop,Stop media playback";
      "org_kde_powerdevil"."Decrease Keyboard Brightness" = "Keyboard Brightness Down";
      "org_kde_powerdevil"."Decrease Screen Brightness" = "none,Monitor Brightness Down,Decrease Screen Brightness";
      "org_kde_powerdevil"."Decrease Screen Brightness Small" = "none,Shift+Monitor Brightness Down,Decrease Screen Brightness by 1%";
      "org_kde_powerdevil"."Hibernate" = "none,Hibernate,Hibernate";
      "org_kde_powerdevil"."Increase Keyboard Brightness" = "Keyboard Brightness Up";
      "org_kde_powerdevil"."Increase Screen Brightness" = "none,Monitor Brightness Up,Increase Screen Brightness";
      "org_kde_powerdevil"."Increase Screen Brightness Small" = "none,Shift+Monitor Brightness Up,Increase Screen Brightness by 1%";
      "org_kde_powerdevil"."PowerDown" = "none,Power Down,Power Down";
      "org_kde_powerdevil"."PowerOff" = "none,Power Off,Power Off";
      "org_kde_powerdevil"."Sleep" = "Sleep";
      "org_kde_powerdevil"."Toggle Keyboard Backlight" = "Keyboard Light On/Off";
      "org_kde_powerdevil"."Turn Off Screen" = [ ];
      "org_kde_powerdevil"."powerProfile" = ["none,Battery" "Meta+B,Switch Power Profile"];
      "plasmashell"."activate application launcher" = ["Meta" "Alt+F1,Meta" "Alt+F1,Activate Application Launcher"];
      "plasmashell"."activate task manager entry 1" = "none,Meta+1,Activate Task Manager Entry 1";
      "plasmashell"."activate task manager entry 10" = "none,Meta+0,Activate Task Manager Entry 10";
      "plasmashell"."activate task manager entry 2" = "none,Meta+2,Activate Task Manager Entry 2";
      "plasmashell"."activate task manager entry 3" = "none,Meta+3,Activate Task Manager Entry 3";
      "plasmashell"."activate task manager entry 4" = "none,Meta+4,Activate Task Manager Entry 4";
      "plasmashell"."activate task manager entry 5" = "none,Meta+5,Activate Task Manager Entry 5";
      "plasmashell"."activate task manager entry 6" = "none,Meta+6,Activate Task Manager Entry 6";
      "plasmashell"."activate task manager entry 7" = "none,Meta+7,Activate Task Manager Entry 7";
      "plasmashell"."activate task manager entry 8" = "none,Meta+8,Activate Task Manager Entry 8";
      "plasmashell"."activate task manager entry 9" = "none,Meta+9,Activate Task Manager Entry 9";
      "plasmashell"."clear-history" = "none,,Clear Clipboard History";
      "plasmashell"."clipboard_action" = "none,Meta+Ctrl+X,Automatic Action Popup Menu";
      "plasmashell"."cycle-panels" = "none,Meta+Alt+P,Move keyboard focus between panels";
      "plasmashell"."cycleNextAction" = "none,,Next History Item";
      "plasmashell"."cyclePrevAction" = "none,,Previous History Item";
      "plasmashell"."manage activities" = "none,Meta+Q,Show Activity Switcher";
      "plasmashell"."next activity" = [ ];
      "plasmashell"."previous activity" = [ ];
      "plasmashell"."repeat_action" = "none,Meta+Ctrl+R,Manually Invoke Action on Current Clipboard";
      "plasmashell"."show dashboard" = "none,Ctrl+F12,Show Desktop";
      "plasmashell"."show-barcode" = "none,,Show Barcode…";
      "plasmashell"."show-on-mouse-pos" = "none,Meta+V,Show Clipboard Items at Mouse Position";
      "plasmashell"."stop current activity" = "none,Meta+S,Stop Current Activity";
      "plasmashell"."switch to next activity" = "none,,Switch to Next Activity";
      "plasmashell"."switch to previous activity" = "none,,Switch to Previous Activity";
      "plasmashell"."toggle do not disturb" = "none,,Toggle do not disturb";
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
  xdg.configFile."plasma-workspace/env/bspwm.sh".text = ''
        #!/bin/sh
         export KDEWM=bspwm
  '';

  xdg.configFile."systemd/user/plasma-bspwm.service" = {
    text = builtins.readFile "${dotfilesDir}/windowManagers/bspwm/plasma_wm_replace/plasma-bspwm.service";
    executable = false;
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
