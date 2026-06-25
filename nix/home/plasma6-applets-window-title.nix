{ pkgs, lib, ... }:

pkgs.stdenv.mkDerivation rec {
  pname = "plasma6-applets-window-title";
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
     mkdir -p $out/share/plasma/plasmoids/org.kde.windowtitle
     cp -r * $out/share/plasma/plasmoids/org.kde.windowtitle
     '';

  meta = with lib; {
    description = "Plasma 6 applet that shows the application title and icon for activ
 window";
    homepage = "https://github.com/dhruv8sh/plasma6-window-title-applet";
    license = licenses.gpl3;
    maintainers = with maintainers; [ dr460nf1r3 ];
  };
}
