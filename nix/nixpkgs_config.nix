{ config, pkgs, lib, ... }@args:

let
  # Get source for unstable.
  # 1. Flake Input: Check if 'inputs' was passed, AND if it has 'nixpkgs-unstable'
  # Check Flake Inputs for multiple possible names
  flakeInput =
    if !(args ? inputs) then null
    else if args.inputs ? nixpkgs-unstable then args.inputs.nixpkgs-unstable
    else if args.inputs ? unstable then args.inputs.unstable
    else null;
  hasFlake = flakeInput != null;

  # 2. Channel: Safely attempt to evaluate the <unstable> channel
  # If the channel doesn't exist, success will be false instead of crashing.
  # We try <nixpkgs-unstable> first, then <unstable>
  channelUnstable = builtins.tryEval <nixpkgs-unstable>;
  channelAlt = builtins.tryEval <unstable>;

  channelSource =
    if channelUnstable.success then channelUnstable.value
    else if channelAlt.success then channelAlt.value
    else null;

  # 3. Resolve the path based on hierarchy
  unstableSource =
    if hasFlake then
      flakeInput
    else if channelSource != null then
      channelSource
    else
      # 4. Fallback: fetchTarball
      builtins.fetchTarball "https://github.com/NixOS/nixpkgs/archive/nixos-unstable.tar.gz";

in
{
  nixpkgs.config = {
    allowUnfree = true;

    # Add unstable as an alias within pkgs, so long as this file is imported.
    packageOverrides = pkgs: {
      unstable = import unstableSource {
        # pass the nixpkgs config to the unstable alias to ensure `allowUnfree = true;` is propagated for unstable.
        config.allowUnfree = true;
      };
    };
  };
}
