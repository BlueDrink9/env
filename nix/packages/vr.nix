
{ config, pkgs, lib, ... }:

with pkgs; let
in
  {

  environment.systemPackages = with pkgs; [
   unstable.wlx-overlay-s
   unstable.opencomposite
    # Only use packaeg. Option needed for loopback and sound but requiares recompiling
    # immersed
  # (GPUOffloadApp steam "steam")
  ];

  # programs.steam = {
  #   enable = true;
  #   remotePlay.openFirewall = true;
  #   localNetworkGameTransfers.openFirewall = true;
  # };

  # boot.kernelModules = "snd-aloop"; # Allow immersed sound
  # programs.immersed.enable = true;

  # programs.alvr.enable = true;
  # programs.alvr.openFirewall = true;
  services.wivrn = {
    enable = true;
    openFirewall = true;
    # Override with this for nvidia
    # services.wivrn.package = pkgs.unstable.wivrn.override { config.cudaSupport = true; };
    package = lib.mkDefault unstable.wivrn;
    # Write information to /etc/xdg/openxr/1/active_runtime.json, VR applications
    # will automatically read this and work with WiVRn (Note: This does not currently
    # apply for games run in Valve's Proton)
    defaultRuntime = true;
    # Run WiVRn as a systemd service on startup
    autoStart = false;
    # Config for WiVRn (https://github.com/WiVRn/WiVRn/blob/master/docs/configuration.md)
    config = {
      enable = true;
      json = {
        # 1.0x foveation scaling
        scale = 1.0;
        # 100 Mb/s
        bitrate = 100000000;
        encoders =lib.mkDefault [
          {
            encoder = "vaapi";
            codec = "h265";
            # 1.0 x 1.0 scaling
            width = 1.0;
            height = 1.0;
            offset_x = 0.0;
            offset_y = 0.0;
          }
        ];
      };
    };
  };
  services.monado.enable = true;
  programs.envision.enable = true;
  programs.envision.openFirewall = true;
  programs.envision.package = unstable.envision;

}
