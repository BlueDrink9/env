#!/usr/bin/env bash
source "$DOTFILES_DIR/shell/script_functions.sh"
installID="Bspwm"
installText=". \"$($SCRIPTDIR_CMD)/bspwmrc\""
configDir="${HOME}/.config/bspwm"
baseRC="${configDir}/bspwmrc"

eval "$(cat <<END
do${installID}() {
    printErr "Enabling bspwm config..."
    # prependTextIfAbsent '#!/bin/sh' "${baseRC}"
    addTextIfAbsent '#!/bin/sh' "${baseRC}"
    addTextIfAbsent "${installText}" "${baseRC}"
    chmod u+x "${baseRC}"
    mkdir -p ~/.config/sxhkd
    touch ~/.config/sxhkd/sxhkdrc
    replace_plasma_wm
}
END
)"

replace_plasma_wm(){
  # Required for KDE 5.25 and newer.
  # Taken from https://maxnatt.gitlab.io/posts/kde-plasma-with-i3wm/#kde-525-and-newer
  # Two methods. The systemd masking doesn't always work, so this is the
  # backup option.
  # KDEWM method
  kwriteconfig5 --file startkderc --group General --key systemdBoot false
  kde_env_dir="$XDG_CONFIG_HOME/plasma-workspace/env/"
  mkdir -p "$kde_env_dir"
  printf '#!/bin/sh\nexport KDEWM=bspwm' >| "$kde_env_dir/bspwm.sh"
  # systemd mask method
  user_sysd_dir="$XDG_CONFIG_HOME/systemd/user/"
  mkdir -p "$user_sysd_dir"
  cp "$($SCRIPTDIR_CMD)/plasma_wm_replace/plasma-bspwm.service" "$user_sysd_dir"
  systemctl --user mask plasma-kwin_x11.service
  systemctl --user daemon-reload
  systemctl --user enable plasma-bspwm.service
  systemctl --user add-wants plasma-workspace-x11.target plasma-bspwm.service

}

eval "$(cat <<END
undo${installID}(){
  sed -in "s|.*${installText}.*||g" "${baseRC}"
}
END
)"

# If directly run instead of sourced, do all
if [ ! "${BASH_SOURCE[0]}" != "${0}" ]; then
    do${installID}
fi
