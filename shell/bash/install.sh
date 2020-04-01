#!/usr/bin/env bash
source "$DOTFILES_DIR/shell/script_functions.sh"

installID="Bash"
installText="source $($SCRIPTDIR_CMD)/bashrc"
baseRC="${HOME}/.bashrc"

eval "$(cat <<END
do${installID}() {
  printErr "Enabling custom bash setup..."
  addTextIfAbsent "source $HOME/.bashrc" "${HOME}/.bash_profile"
  addTextIfAbsent "${installText}" "${baseRC}"
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

installLiquidprompt(){
  printErr "Downloading liquidprompt..."
  git clone --depth 1 https://github.com/nojhan/liquidprompt.git ~/.config/liquidprompt
  addTextIfAbsent "source \"$($SCRIPTDIR_CMD)/prompt/liquidprompt/liquidpromptrc\"" "${HOME}/.config/liquidpromptrc"
}

eval "$(cat <<END
undo${installID}(){
  # The sed commands replace source lines with blanks
  sed -in "s|.*${installText}.*||g" "${baseRC}"
  sed -in "s|source $HOME/.bashrc||g" "${HOME}/.bash_profile"
}
END
)"

# If directly run instead of sourced, do all
if [ ! "${BASH_SOURCE[0]}" != "${0}" ]; then
  doBash
fi
