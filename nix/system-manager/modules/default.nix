{ config, lib, pkgs, ... }:

{
  imports = [
    # /etc/local-packages.nix
      /etc/system-manager.nix
    # ./vr.nix
    ./hardware/tobii5.nix
  ];
  config = {
    nixpkgs.hostPlatform = "x86_64-linux";
    system-manager.allowAnyDistro = true;

    # environment.sessionVariables = rec {
    # };

    environment.systemPackages =
      with pkgs;
      let
        reboot_to_windows_script = (
          writeShellScriptBin "reboot_to_windows" ''
            sudo ${pkgs.systemd}/bin/bootctl set-oneshot auto-windows && reboot
          ''
        );

      in
      [

        reboot_to_windows_script
        (makeDesktopItem {
          name = "Reboot to Windows";
          desktopName = "Reboot to Windows";
          exec = "${reboot_to_windows_script}/bin/reboot_to_windows";
          icon = ./img/win10_coloured.ico;
          terminal = true;
        })

        (pkgs.writeScriptBin "reset-tobii-hub" (
          builtins.readFile ../../../shell/scripts/reset-tobii-hub.py
        ))

        efibootmgr
        # set_next_boot
        #writeShellScriptBin adds shebang, writeScriptBin doesn't. Testing which is better.
        (writeShellScriptBin "reboot_to_firmware" ''
          sudo ${pkgs.systemd}/bin/bootctl set-oneshot auto-reboot-to-firmware-setup && reboot
        '')

      ];

    systemd.services = {
    };

    environment.etc."sudoers.d/system-manager-options".text = ''
      # Check system manager bin dir, for pwless scripts run as just their bin name rather than full path.
      Defaults secure_path="/run/system-manager/sw/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin"
    '';

    # eg
    # environment.etc."sudoers.d/pwless_scripts".text = ''
    #   my_user ALL = (root) NOPASSWD: /run/system-manager/sw/bin/reset-tobii-hub
    #   my_user ALL = (root) NOPASSWD: /run/system-manager/sw/bin/reboot_to_windows
    #   my_user ALL = (root) NOPASSWD: /run/system-manager/sw/bin/reboot_to_firmware
    # '';

    environment.etc."profile.d/editor".text = ''
      export VISUAL=${pkgs.neovim}/bin/nvim
    '';

    # system.activationScripts.udev.text = ''
    #   #!${pkgs.runtimeShell}
    #   PATH=$PATH:${lib.makeBinPath [ pkgs.systemd ]}
    #   udevadm control --reload-rules
    #   udevadm trigger
    # '';

  };
}
