{pkgs, ...}:
# SELinux policy file
let
  policy-file = pkgs.writeText "allow-system-manager.te" ''
    module allow-system-manager 1.0;
    require {
            type default_t;
            type tmpfs_t;
            type ifconfig_t;
            type init_t;
            type systemd_unit_file_t;
            class cap_userns net_admin;
            class lnk_file read;
            class file { execute execute_no_trans map open read };
    }
    #============= ifconfig_t ==============
    allow ifconfig_t self:cap_userns net_admin;
    allow ifconfig_t tmpfs_t:lnk_file read;
    #============= init_t ==============
    allow init_t default_t:file map;
    allow init_t default_t:file { execute execute_no_trans open read };
    allow init_t default_t:lnk_file read;
    # Allow systemd to read systemd unit files with default_t context
    allow init_t default_t:file read;
  '';
  # Pre-compiled SELinux policy package
  policy-package =
    pkgs.runCommand "allow-system-manager.pp" {
      buildInputs = [pkgs.policycoreutils pkgs.checkpolicy pkgs.semodule-utils];
    } ''
      checkmodule -M -m -o allow-system-manager.mod ${policy-file}
      semodule_package -o $out -m allow-system-manager.mod
    '';
  install-selinux-policy = pkgs.writeShellApplication {
    name = "install-selinux-policy";
    runtimeInputs = [pkgs.libsemanage pkgs.policycoreutils];
    text = ''
      set -e
      # Remove old module if exists
      sudo semodule -r allow-system-manager 2>/dev/null || true
      # Install pre-built policy package
      sudo semodule -i ${policy-package}
      # Fix contexts and reload
      sudo restorecon -R /etc/systemd/system/
      sudo systemctl daemon-reload
      echo "SELinux policy installed"
    '';
  };
in
  install-selinux-policy
