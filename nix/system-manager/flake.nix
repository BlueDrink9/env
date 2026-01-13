{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

    system-manager = {
      url = "github:numtide/system-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = {
    self
    , flake-utils
    , nixpkgs
    , system-manager
    , ...
  }@inputs: {
    systemConfigs.default = system-manager.lib.makeSystemConfig {
      modules = [
        {
          nix.settings.experimental-features = "nix-command flakes";
          nix.settings.extra-substituters = https://cache.numtide.com;
          nix.settings.extra-trusted-public-keys = niks3.numtide.com-1:DTx8wZduET09hRmMtKdQDxNNthLQETkc/yaX7M4qK0g=;
        }
        ./modules
      ];
    };
  };
}
