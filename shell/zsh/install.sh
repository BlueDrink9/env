#!/usr/bin/env bash
source "$DOTFILES_DIR/shell/script_functions.sh"

installID="Zsh"
installText="source $($SCRIPTDIR_CMD)/zshrc"
baseRC="${HOME}/.zshrc"

eval "$(cat <<END
do${installID}() {
  printErr "Enabling custom zsh setup..."
  # addTextIfAbsent "source $HOME/.bashrc" "${HOME}/.bash_profile"

  installBase16Shell
  installZSHPlugins
  # Do after installing plugins
  addTextIfAbsent "${installText}" "${baseRC}"
}
END
)"

installBase16Shell(){
  printErr "Downloading base16-shell..."
  git clone https://github.com/chriskempson/base16-shell.git ~/.config/base16-shell
}

installZSHPlugins(){
  printErr "Downloading zinit..."
  local DIR="${XDG_DATA_HOME:-$HOME/.local/share}/zinit"
  mkdir -p "$DIR"
  git clone https://github.com/zdharma/zinit "$DIR"/bin
}

installLiquidprompt(){
  printErr "Downloading liquidprompt..."
  git clone https://github.com/nojhan/liquidprompt.git ~/.config/liquidprompt
  addTextIfAbsent "source \"$($SCRIPTDIR_CMD)/prompt/liquidprompt/liquidpromptrc\"" "${HOME}/.config/liquidpromptrc"
}

eval "$(cat <<END
undo${installID}(){
  # The sed commands replace source lines with blanks
  sed -in "s|.*${installText}.*||g" "${baseRC}"
  sed -in "s|source $HOME/.bashrc||g" "${HOME}/.bash_profile"
  rm -rf "${HOME}/.dircolours_solarized"
  sed -in "s|.*$($SCRIPTDIR_CMD)/inputrc.sh.*||g" "${HOME}/.inputrc"
}
END
)"

# If directly run instead of sourced, do all
if [ ! "${BASH_SOURCE[0]}" != "${0}" ]; then
  doZsh
fi
