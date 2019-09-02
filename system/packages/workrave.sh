#!/usr/bin/env bash
pushd $HOME
mkdir -p ~/Applications ~/.local/build ~/.local/src
rm -rf ~/Library/Preferences/com.workrave.plist
rm -rf ~/Library/Preferences/workrave.plist
# Officially required packages:
wr_dependencies_official="gettext intltool gobject-introspection autoconf-archive gtk+ gtk-mac-integration gtkmm3"
# These packages get it working without crashes, with caveat of no symbols/icons. IDK which one does the trick.
wr_dependencies_extra="automake autoconf libtool gnome-icon-theme hicolor-icon-theme"
brew install $wr_dependencies_official $wr_dependencies_extra
brew link --force gettext libffi
git clone https://github.com/palfrey/workrave/ "$HOME/.local/src/workrave"
cd workrave
git checkout os-x-fixes
autoreconf -if
intltoolize --automake --copy --force
./configure --prefix="$HOME/.local/build/workrave" --config-cache
make
make install
stow  --dir="$HOME/.local/build" workrave --target="$HOME/.local"
popd
