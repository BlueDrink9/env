{ pkgs, lib, ... }:

pkgs.stdenv.mkDerivation rec {
  pname = "mac-app-menu";
  version = "0.10.0";
  src = pkgs.fetchFromGitHub {
    owner = "ajxcodes";
    repo = "mac-app-menu";
    tag = "v${version}";
    sha256 = "sha256-7vLxxUmfqMn4F1ZiTrtZIFpLJifpfm1NGZkWwGyJLfc=";
  };


  installPhase = ''
    # Places in .nix-profile/share. Plasmashell will pick it up if that dir is
    # in $XDG_DATA_DIRS, which it should be if sourcing
    # $HOME/.nix-profile/etc/profile.d/hm-session-vars.sh
     mkdir -p $out/share/plasma/plasmoids/com.ajxcodes.macappmenu
     cp -r * $out/share/plasma/plasmoids/com.ajxcodes.macappmenu
     '';

  meta = with lib; {
    description = "Plasma 6 applet that shows the application title and icon for activ
 window";
    homepage = "https://github.com/ajxcodes/mac-app-menu";
    license = licenses.gpl3;
    maintainers = with maintainers; [ ];
  };
}
