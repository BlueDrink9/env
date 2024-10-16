{ pkgs, lib, ... }:

pkgs.stdenv.mkDerivation rec {
  pname = "sweet-kde";
  version = "master";

  src = pkgs.fetchurl {
    url = "https://github.com/EliverLara/Sweet-kde/archive/master.tar.gz";
    sha256 = "sha256-auBAKEutZT5hGLN1vdpTzw4rVvQd8J7XoT1fcj8cdpc=";
  };

  installPhase = ''
  pushd src
  while read $line; do echo -n "."; done < \
  <(./render-gtk3-assets.py 2>/dev/null; \
  ./render-gtk3-assets-hidpi.py 2>/dev/null; \
  ./render-wm-assets-hidpi.py 2>/dev/null; \
  ./render-wm-assets.py 2>/dev/null); echo
  popd
  mkdir -p $out/user/share/themes/sweet-kde
  cp -r user/share/themes/sweet-kde/* $out/share/plasma/plasmoids/
  '';

  meta = with lib; {
    description = "A dark and moduern theme for Plasma based on the awesome Helium theme.";
    homepage = "https://store.kde.org/p/1294174";
    maintainers = with maintainers; [ EliverLara ];
    platforms = platforms.linux;
    licene = licenses.gpl3;
  };
}
