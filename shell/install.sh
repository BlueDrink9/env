#!/usr/bin/env bash
source "$DOTFILES_DIR/shell/script_functions.sh"
source "$($SCRIPTDIR_CMD)/bash/install.sh"
source "$($SCRIPTDIR_CMD)/zsh/install.sh"

installID="Shell"

eval "$(cat <<END
do${installID}() {
  printErr "Enabling custom shell setup..."
  doBash
  doZsh

  installDircolours
  installBase16Shell
  doReadline
}
END
)"

installDircolours(){
  printErr "Downloading dircolours_solarized..."
  downloadURLtoFile  \
      https://raw.githubusercontent.com/seebi/dircolors-solarized/master/dircolors.256dark  \
      "${HOME}/.dircolours_solarized"
  # https://raw.githubusercontent.com/seebi/dircolors-solarized/master/dircolors.ansi-universal
}

installBase16Shell(){
  printErr "Downloading base16-shell..."
  git clone https://github.com/chriskempson/base16-shell.git ~/.config/base16-shell
}

eval "$(cat <<END
undo${installID}(){
  # The sed commands replace source lines with blanks
  rm -rf "${HOME}/.dircolours_solarized"
  sed -in "s|.*$($SCRIPTDIR_CMD)/inputrc.sh.*||g" "${HOME}/.inputrc"
}
END
)"

installID="Readline"
installText="\\\$include $($SCRIPTDIR_CMD)/inputrc"
installText="${installText}
\\\$include $HOME/.readline-surround"
baseRC="${HOME}/.inputrc"

eval "$(cat <<END
do${installID}() {
  printErr "Enabling custom readline (inputrc) setup..."
  downloadURLtoFile https://raw.githubusercontent.com/liloman/bash-surround/master/inputrc-surround $HOME/.readline-surround
  addTextIfAbsent "${installText}" "${baseRC}"
}
END
)"

# If directly run instead of sourced, do all
if [ ! "${BASH_SOURCE[0]}" != "${0}" ]; then
  doShell
  # doReadline
fi
