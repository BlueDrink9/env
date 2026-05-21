#!/usr/bin/env bash
source "$DOTFILES_DIR/shell/script_functions.sh"

doJJ() {
  jjSettings
  if [ "$LITE" = 1 ]; then
    return
  fi
  jjUser
}

jjUser() {
  jj config set --user user.name "$(git config --global user.name)"
  jj config set --user user.email "$(git config --global user.email)"
}

jjSettings() {
  printErr "Enabling custom jj setup..."
  TARGET="$XDG_CONFIG_HOME/jj/conf.d/env_config.toml"
  mkdir -p $(dirname "$TARGET")
  if [ ! -L "$TARGET" ]; then
    ln -s "$DOTFILES_DIR/misc/jj/jjconfig.toml" "$TARGET"
  fi
  unset TARGET
  # git config --global core.excludesfile "$($SCRIPTDIR_CMD)/gitignore"
  # git config --global core.attributesfile "$($SCRIPTDIR_CMD)/gitattributes"
}

# If directly run instead of sourced, do all
if [ ! "${BASH_SOURCE[0]}" != "${0}" ]; then
  doJJ "$@"
fi
