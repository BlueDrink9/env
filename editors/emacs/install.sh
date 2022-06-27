#!/usr/bin/env bash
source "$DOTFILES_DIR/shell/XDG_setup.sh"
export DOOMDIR="$DOTFILES_DIR/editors/emacs/doom.d"
installID="Emacs"
# installText="(load! \"$DOTFILES_DIR/editors/emacs/doom_config.el\")"
# baseRC="~/.doom.d/config.el"
installText='(setenv \"DOOMDIR\" \"'$DOOMDIR'\")'
baseRC="$HOME/.emacs.d/early-init.el"

source "$DOTFILES_DIR/shell/script_functions.sh"
source "$DOTFILES_DIR/shell/functions.sh"

# printErr "Enabling custom ${installID} setup..."
eval "$(cat <<END
do${installID}() {
    printErr "Setting up doom emacs"
    # installDoomEmacs
  # Put on second line, so that lexical binding still works.
    prependTextIfAbsent "${installText}" "${baseRC}" 2
  }
END
)"

installDoomEmacs(){
  # Use emacs.d because it's where packages are installed, and I don't want them taking up backup space in ~/.config
  if [ ! -f ~/.emacs.d/bin/doom ]; then
    rm -rf ~/.emacs.d
    git clone --depth 1 https://github.com/hlissner/doom-emacs ~/.emacs.d
  else
    git -C ~/.emacs.d pull
    ~/.emacs.d/bin/doom upgrade
  fi
  ln -s ~/.emacs.d/bin/doom ~/.local/bin/doom
  ~/.emacs.d/bin/doom install
  # If git bash, set user environment variable $DOOMDIR and $EMACS_SERVER_FILE
  if [[ $OSTYPE == 'msys' ]]; then
    powershell.exe -command "[System.Environment]::SetEnvironmentVariable('DOOMDIR', '$(cygpath.exe -w $DOOMDIR)', [System.EnvironmentVariableTarget]::User)"
    powershell.exe -command "[System.Environment]::SetEnvironmentVariable('EMACS_SERVER_FILE', '$(cygpath.exe -w ~/.emacs.d/.local/cache/server/server)', [System.EnvironmentVariableTarget]::User)"
  fi
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
  installDoomEmacs
fi
