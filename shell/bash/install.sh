#!/usr/bin/env bash
source "$DOTFILES_DIR/shell/bash/script_functions.sh"

installID="Shell"
installText="source $($SCRIPTDIR_CMD)/bashrc"
baseRC="${HOME}/.bashrc"

eval "$(cat <<END
do${installID}() {
  printErr "Enabling custom bash setup..."
  addTextIfAbsent "source $HOME/.bashrc" "${HOME}/.bash_profile"
  addTextIfAbsent "${installText}" "${baseRC}"

  installDircolours
  installBase16Shell
  doReadline
  gitSettings
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
