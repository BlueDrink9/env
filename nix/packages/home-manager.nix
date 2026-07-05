{ config, pkgs, ... }@args:

let
  # Get source for Home Manager.
  # 1. Flake input: check if "home-manager" was passed.
  flakeInput =
    if !(args ? inputs) then
      null
    else if args.inputs ? home-manager then
      args.inputs.home-manager
    else
      null;

  hasFlake = flakeInput != null;

  # 2. Channel: safely evaluate <home-manager>
  channel = builtins.tryEval <home-manager>;

  channelSource =
    if channel.success then
      channel.value
    else
      null;

  # 3. Resolve source based on priority
  homeManagerSource =
    if hasFlake then
      flakeInput
    else if channelSource != null then
      channelSource
    else
      # 4. Fallback: fetchTarball
      builtins.fetchTarball {
        url = "https://github.com/nix-community/home-manager/archive/release-26.05.tar.gz";
        sha256 = "sha256:0xpgskfs8q9jdd0hc8298h1qg2w6i36g0w1mmvyl169lmr8v3zqi";
      };

  hm = import "${homeManagerSource}/nixos";

in
{
  imports = [ hm ];

  home-manager.useGlobalPkgs = true;

  # home-manager.users.${user} = {
  #   imports = [ "${nixDir}/home/home.nix" ];
  # };
}
