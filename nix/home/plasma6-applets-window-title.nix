{ pkgs, lib, ... }:

pkgs.stdenv.mkDerivation rec {
  pname = "plasma6-applets-window-title";
  version = "0.9.0";
  # This is an old one, but it may have a feature that truncates the title if its too big?
  # https://github.com/psifidotos/applet-window-title
  src = pkgs.fetchurl {
    url =
      "https://github.com/dhruv8sh/plasma6-window-title-applet/archive/refs/tags/v${version}.tar.gz";
    sha256 = "82a0ae9d10c47e36c510a45fb8f793891def81addfa32b0697b580a84d18a6c2";
  };

  nativeBuildInputs = [ pkgs.qt6.wrapQtAppsHook ];
  buildInputs = [ pkgs.kdePackages.breeze pkgs.kdePackages.plasma-workspace ];

  installPhase = ''
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
