{ lib, pkgs, config, ... }:
with lib;
let
  cfg = config.my;
in {
  options.my = {
    pkgs = mkOption {
      type = types.listOf types.package;
      default = [];
    };
  };

  config = {
    home.packages = cfg.pkgs;
  };
}
