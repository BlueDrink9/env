{ config, pkgs, lib, ... }:

with pkgs; let
  patchDesktop = pkg: appName: from: to: lib.hiPrio (
    pkgs.runCommand "$patched-desktop-entry-for-${appName}" {} ''
      ${coreutils}/bin/mkdir -p $out/share/applications
      ${gnused}/bin/sed 's#${from}#${to}#g' < ${pkg}/share/applications/${appName}.desktop > $out/share/applications/${appName}.desktop
      '');
  GPUOffloadApp = pkg: desktopName: lib.mkIf config.hardware.nvidia.prime.offload.enable
    (patchDesktop pkg desktopName "^Exec=" "Exec=nvidia-offload ");
in
  {

  environment.systemPackages = with pkgs; [
    heroic
    # cider
    moonlight-qt
    (GPUOffloadApp steam "steam")
    (GPUOffloadApp heroic "com.heroicgameslauncher.hgl")
  ];

  programs.steam = {
    enable = true;
    remotePlay.openFirewall = true;
    localNetworkGameTransfers.openFirewall = true;
  };

  nixpkgs.overlays = [
    (final: prev: {
      steam = prev.steam.overrideAttrs (oldAttrs: {
        postInstall = let
          extraPostInstall = if config.hardware.nvidia.prime.offload.enable then ''
            substituteInPlace $out/share/applications/steam.desktop \
        --replace "Exec=" "Exec=nvidia-offload "
            ${pkgs.gnused}/bin/sed -i 's/^Exec=/&nvidia-offload /' $out/share/applications/*.desktop
          '' else "";
        in
          (oldAttrs.postInstall or "") + extraPostInstall;
      });
    })
  ];


}
