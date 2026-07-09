{ config, pkgs, lib, ... }:

{
  home.packages = with pkgs.xfce; [
    # XFCE panel plugins; for now have to be system installed
    # xfce4-panel
    # xfce4-windowck-plugin
    # xfce4-whiskermenu-plugin
    # # xfce4-mount-plugin
    # xfce4-xkb-plugin
    # xfce4-notifyd
    # xfce4-battery-plugin
    # xfce4-pulseaudio-plugin
    # xfce4-power-manager

  ];

  # programs.xfconf.enable = true;

  xfconf.settings = {
    # Keyboard layouts
    keyboards = {
      "/Default/XkbLayout"  = "us,nz";
      "/Default/XkbVariant" = "colemak,";
      "/Default/XkbOptions" = "grp:win_space_toggle,grp_led:caps";
    };

    # Mouse / pointer
    pointers = {
      "/Default/Acceleration" = 0.1;
    };

    # Power manager (XFCE)
    xfce4-power-manager = {
      "/xfce4-power-manager/lid-action-on-ac"                  = 0;
      "/xfce4-power-manager/power-button-action"              = 2;
      "/xfce4-power-manager/brightness-level-on-ac"           = 100;
      "/xfce4-power-manager/brightness-level-inactivity-on-ac"= 70;
      "/xfce4-power-manager/blank-on-ac"                      = 2;

      "/xfce4-power-manager/lid-action-on-battery"                  = 1;
      "/xfce4-power-manager/brightness-level-on-battery"            = 70;
      "/xfce4-power-manager/brightness-level-inactivity-on-battery" = 70;
      "/xfce4-power-manager/blank-on-battery"                       = 1;
      "/xfce4-power-manager/critical-power-action"                  = 2;
    };

    # Panel (approximation)
    xfce4-panel = {
      "/panels/1/position"          = "p=6;x=0;y=0";
      "/panels/1/size"              = 44;
      "/panels/1/length"            = 100;
      "/panels/1/autohide-behavior" = 0;
    };

    # Session defaults
    xfce4-session = {
      "/compat/TerminalEmulator" = "kitty";
    };

    # Thunar settings
    thunar = {
      "/misc-show-full-path"         = true;
      "/misc-volume-show-progress"   = true;
      "/misc-open-new-window-as-tab" = true;
    };

    # Keyboard shortcuts
    xfce4-keyboard-shortcuts = {
      "/commands/custom/<Primary><Alt>L" = "xflock4";
      # "/commands/custom/Super_L"         = "xfce4-popup-whiskermenu";
      # "/commands/custom/Print"           = "xfce4-screenshooter";
    };

  };

}
