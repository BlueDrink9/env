# https://github.com/MalteT/custom-udev-rules/blob/main/flake.nix
{ pkgs, lib, config, ... }:
let
  createUdevPkg = { name, rules }:
    pkgs.writeTextFile {
      name = "custom-udev-rule-" + name;
      text = rules;
      destination = "/lib/udev/rules.d/" + name + ".rules";
    };
  cfg = config.services.udev.customRules;
in {
  options = {
    services.udev.customRules = lib.mkOption {
      type = lib.types.listOf (lib.types.submodule {
        options = {
          name = lib.mkOption {
            type = lib.types.str;
            default = "99-custom.rules";
            description = "The name of the file";
          };
          rules = lib.mkOption {
            type = lib.types.lines;
            description = "Content of the file";
          };
        };
      });
      default = [ ];
      example = [{
        name = "85-yubikey";
        rules = ''
                SUBSYSTEM=="usb", ENV{ID_MODEL_ID}=="0407", ENV{ID_VENDOR_ID}=="1050", TAG+="systemd", SYMLINK+="yubikey"
        '';
      }];
      description = ''
              Additional custom udev rules
      '';
    };
  };

  config = { services.udev.packages = map createUdevPkg cfg; };
}
