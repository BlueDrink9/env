#!/usr/bin/env bash
source "$DOTFILES_DIR/shell/script_functions.sh"
source "$DOTFILES_DIR/shell/functions.sh"

installID="Tmux"
installText="source-file $($SCRIPTDIR_CMD)/tmux/tmux.conf"
baseRC="${HOME}/.tmux.conf"

eval "$(cat <<END
do${installID}() {
    printErr "Enabling custom ${installID} setup..."
    addTextIfAbsent "${installText}" "${baseRC}"
  }
END
)"

eval "$(cat <<END
undo${installID}(){
    sed -in "s|.*${installText}.*||g" "${baseRC}"
  }
END
)"

installID="Kitty"
installText="include $($SCRIPTDIR_CMD)/kitty/kitty.conf"
baseRC="${HOME}/.config/kitty/kitty.conf"

eval "$(cat <<END
do${installID}() {
    printErr "Enabling custom ${installID} setup..."
    addTextIfAbsent "${installText}" "${baseRC}"
    terminal_kitty_install
  }
END
)"

download_kitty_theme(){
  url="$1"
  downloadFile "$url" "$datadir"/kitty-themes/themes/
}

terminal_kitty_install(){
    local datadir="${XDG_DATA_HOME:-$HOME/.local/share}"/kitty
    mkdir -p "$datadir"
    git clone --depth 1 https://github.com/dexpota/kitty-themes.git "$datadir"/kitty-themes
    download_kitty_theme https://raw.githubusercontent.com/sonph/onehalf/master/kitty/onehalf-light.conf
    download_kitty_theme https://raw.githubusercontent.com/sonph/onehalf/master/kitty/onehalf-dark.conf
    download_kitty_theme https://raw.githubusercontent.com/srcery-colors/srcery-terminal/master/kitty/srcery_kitty.conf srcery.conf
    mv "$datadir"/kitty-themes/themes/srcery_kitty.conf "$datadir"/kitty-themes/themes/srcery.conf
    if ! command -v kitty > /dev/null 2>&1; then
      curl -L https://sw.kovidgoyal.net/kitty/installer.sh | sh /dev/stdin
    fi
}

eval "$(cat <<END
undo${installID}(){
    sed -in "s|.*${installText}.*||g" "${baseRC}"
  }
END
)"

installID="iTerm2"
eval "$(cat <<END
do${installID}() {
    printErr "Enabling custom ${installID} setup..."
    # Specify the preferences directory
    defaults write com.googlecode.iterm2.plist PrefsCustomFolder -string "$($SCRIPTDIR_CMD)/iterm2"
    # Tell iTerm2 to use the custom preferences in the directory
    defaults write com.googlecode.iterm2.plist LoadPrefsFromCustomFolder -bool true
  }
END
)"
eval "$(cat <<END
undo${installID}(){
    true
  }
END
)"

installID="Alacritty"
installText="import: \n\
  - $($SCRIPTDIR_CMD)/alacritty/alacritty.yml"
baseRC="${XDG_CONFIG_HOME:-$HOME/.config}/alacritty.yml"

eval "$(cat <<END
do${installID}() {
    printErr "Enabling custom ${installID} setup..."
    addTextIfAbsent "${installText}" "${baseRC}"
  }
END
)"

eval "$(cat <<END
undo${installID}(){
    sed -in "s|.*${installText}.*||g" "${baseRC}"
  }
END
)"

installID="Termux"
eval "$(cat <<END
do${installID}() {
    printErr "Enabling custom ${installID} setup..."
    . "$($SCRIPTDIR_CMD)/termux/setup.sh"
  }
END
)"
eval "$(cat <<END
undo${installID}(){
    rm -rf "$HOME/.termux"
  }
END
)"

# If directly run instead of sourced, do all
if [ ! "${BASH_SOURCE[0]}" != "${0}" ]; then
  doTmux
  if substrInStr "Android" "$(uname -a)";  then
    doTermux
  elif [ "$OSTYPE" != "msys" ]; then
    doKitty
    doAlacritty
  fi
fi
