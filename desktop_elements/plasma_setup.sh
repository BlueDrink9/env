#!/usr/bin/env bash
# source "$DOTFILES_DIR/shell/script_functions.sh"
. "$DOTFILES_DIR/shell/functions.sh"

set -ue

applet_dir="$XDG_DATA_HOME/plasma/applets"
mkdir -p "$applet_dir"
applet_config="$XDG_CONFIG_HOME/plasma-org.kde.plasma.desktop-appletsrc"

install_applet(){
  url="$1"
  dir="$applet_dir/$(basename "$url")"
  rm -rf "$dir"
  git clone --depth=1 "$url" "$dir"
  # Or with true in case already installed
  kpackagetool6 --type Plasma/Applet --install "$dir" || true
}

find_applet_groups(){
  applet="$1"
  # default to finding the first matching applet. Use higher counts if there
  # are multiple instances of the panel.
  count="${2:-1}"

  found=0
  groups=""
  while IFS="" read -r line; do
    if substrInStr Applets "$line"; then
      # Save the line that begins each Applet group
      groups="$line"
    fi
    if substrInStr "$applet" "$line"; then
      found=$((found+1))
      if [ "$found" -eq "$count" ]; then
        # Found the group for the desired applet.
        break
      fi
    fi
  done < "$applet_config"
  ContGrp=$(echo "$groups" | awk -F\] '{print $2}' | awk -F\[ '{print $2}')
  ApplGrp=$(echo "$groups" | awk -F\] '{print $4}' | awk -F\[ '{print $2}')
  printf "%s\t%s" "$ContGrp" "$ApplGrp"
}

configure_applet(){
  applet="$1"
  confgroup="$2"
  key="$3"
  value="$4"

  groups="$(find_applet_groups $applet)"
  ContGrp="$(echo "$groups" | cut -f1)"
  ApplGrp="$(echo "$groups" | cut -f2)"

  set -x
  kwriteconfig6 --file "$(basename "$applet_config")" \
    --group Containments --group "$ContGrp" --group Applets --group "$ApplGrp" \
    --group Configuration --group $confgroup \
    --key "$key" "$value"
  set +x
}

append_applet_id(){
  applet=$1
  base=$2
  count=${3:-1}  # Default to 1
  next="$(find_applet_groups $applet $count | cut -f2)"
  echo "$base;$next"
}

eval_plasma_script(){
  qdbus org.kde.plasmashell /PlasmaShell org.kde.PlasmaShell.evaluateScript "$@"
}
extract_json_value(){
  key=$1
  out="$(eval_plasma_script "print(JSON.parse('"$jsonOutput"').$key)")"
  if [ -z "$out" ]; then
    echo "Error: Key not found: $key" >&2
    exit 1
  fi
  printf "$out"
}


install_panel_widgets(){
  # Add the windowtitle widget
  contents=$(<"$DOTFILES_DIR/desktop_elements/plasma_panel_add_widget.js")
  jsonOutput="$(eval_plasma_script "$contents")"
  # echo $jsonOutput
  panel_id=$(extract_json_value "panel_id")
  order=$(extract_json_value "order")
  # The new config doesn't get the new applets until a reload is
  # triggered, so need to return the ids to bash, reload, then set the order.
  qdbus org.kde.plasmashell /PlasmaShell org.kde.PlasmaShell.refreshCurrentShell
  set -x
  kwriteconfig6 --file "$(basename "$applet_config")" \
    --group Containments --group "${panel_id}" --group General \
    --key "AppletOrder" "${order}"
  set +x
  # Reload again to show the new layout
  qdbus org.kde.plasmashell /PlasmaShell org.kde.PlasmaShell.refreshCurrentShell

  return
  # The js above does the ordering below this better/more reliably, as well as adding the
  # new widgets.

  groups=""
  while IFS="" read -r line; do
    if substrInStr Containments "$line"; then
      # Save the line that begins each Applet group
      groups="$line"
    elif substrInStr "org.kde.panel" "$line"; then
      # Found the group for the desired applet.
      break
    fi
  done < "$applet_config"
  ContGrp=$(echo "$groups" | awk -F\] '{print $2}' | awk -F\[ '{print $2}')

  first="$(find_applet_groups org.kde.plasma.kickoff | cut -f2)"  # launcher
  order=$(append_applet_id org.kde.plasma.pager "$first")
  order=$(append_applet_id org.kde.plasma.panelspacer "$order")
  order=$(append_applet_id org.kde.windowtitle "$order")
  order=$(append_applet_id org.kde.plasma.panelspacer "$order" 2)
  order=$(append_applet_id org.kde.plasma.systemtray "$order")
  order=$(append_applet_id org.kde.plasma.digitalclock "$order")
  # order=$(append_applet_id org.kde.plasma.showdesktop "$order")
  # order=$(append_applet_id org.kde.plasma.panelspacer "$order")

  set -x
  kwriteconfig6 --file "$(basename "$applet_config")" \
    --group Containments --group "$ContGrp" --group General \
    --key "AppletOrder" "$order"
  set +x
  qdbus org.kde.plasmashell /PlasmaShell org.kde.PlasmaShell.refreshCurrentShell
}

modify_shortcut(){
  kwriteconfig6 --file kglobalshortcutsrc "$@"
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

install_applet https://github.com/dhruv8sh/plasma6-window-title-applet
install_panel_widgets

disable_plasmashell_task_manager_overrides
disable_other_meta_shortcuts

# plasma-apply-wallpaperimage /path

# look and feel(global theme): plasma-apply-lookandfeel "your favorite global theme"
# color scheme: plasma-apply-colorscheme "your favorite color scheme"
# cursor theme: plasma-apply-cursortheme "your favorite cursor"
# desktop theme(plasma theme?): plasma-apply-desktoptheme "your favorite desktop theme"
# bonus: accent color: plasma-apply-colorscheme --accent-color "svg color name or hex color code"
