#!/usr/bin/env bash
source "$DOTFILES_DIR/shell/script_functions.sh"

installID="Zsh"
installText="source $($SCRIPTDIR_CMD)/zshrc"
baseRC="${HOME}/.zshrc"

eval "$(cat <<END
do${installID}() {
  printErr "Enabling custom zsh setup..."
  addTextIfAbsent "source $HOME/.zshrc" "${HOME}/.zsh_profile"

  installZSHPlugins
  # Do after installing plugins
  addTextIfAbsent "${installText}" "${baseRC}"
}
END
)"

installZSHPlugins(){
  printErr "Downloading zinit..."
  local DIR="${XDG_DATA_HOME:-$HOME/.local/share}/zinit"
  mkdir -p "$DIR"
  git clone --depth 1 https://github.com/zdharma/zinit "$DIR"/bin
}

eval "$(cat <<END
undo${installID}(){
  # The sed commands replace source lines with blanks
  sed -in "s|.*${installText}.*||g" "${baseRC}"
  sed -in "s|source $HOME/.zshrc||g" "${HOME}/.zsh_profile"
}
END
)"

# If directly run instead of sourced, do all
if [ ! "${BASH_SOURCE[0]}" != "${0}" ]; then
  doZsh
fi
