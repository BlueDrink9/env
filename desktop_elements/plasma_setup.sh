#!/bin/sh

set -e
applet_dir="$XDG_DATA_HOME/plasma/applets"
mkdir -p "$applet_dir"

install_applet(){
  url="$1"
  dir="$applet_dir/$(basename "$url")"
  rm -rf "$dir"
  git clone --depth=1 "$url" "$dir"
  kpackagetool5 -i "$dir"
}
install_applet https://github.com/psifidotos/applet-window-title
install_applet https://gitlab.com/aleixq/apptitle-plasmoid

modify_shortcut(){
  kwriteconfig5 --file kglobalshortcutsrc $@
}

disable_plasmashell_task_manager_overrides(){
  # Disable the use of meta 1-9 + 0 by plasmashell, to ensure window manager can
  # use it.
  i=1
  while [ "$i" -le 10 ]
  do
    entry=$i
    key=$i
    if [ "$i" -eq 10 ]; then
      key=0
    fi
    # Leaves the option in settings, but sets it as inactive.
    replace="none,Meta+$key,Activate Task Manager Entry $entry"
    modify_shortcut --group plasmashell --key "activate task manager entry $entry" "$replace"
    i=$((i + 1))
  done
}

disable_other_meta_shortcuts(){
  modify_shortcut --group "org.kde.dolphin.desktop" --key "_launch" "none,Meta+E,Dolphin"
}

disable_plasmashell_task_manager_overrides
