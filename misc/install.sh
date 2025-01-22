#!/usr/bin/env bash
# Source these for required functions and XDG variables (if used)
source "$DOTFILES_DIR/shell/script_functions.sh"
source "$DOTFILES_DIR/shell/functions.sh"
source "$DOTFILES_DIR/shell/XDG_setup.sh"

installID="Vieb"
installText="source $($SCRIPTDIR_CMD)/viebrc"
baseRC="${HOME}/.vieb/viebrc"
source "$DOTFILES_DIR/generic_rc_installer.sh"

installID="Pyvim"
# installText="with open(r'$DOTFILES_DIR/editors/pyvim/pyvimrc') as infile: exec(infile.read())"
installText="from importlib.machinery import SourceFileLoader\nconfigure = SourceFileLoader('mypyvimrc', '$DOTFILES_DIR/editors/pyvim/pyvimrc').load_module().configure"
baseRC="${HOME}/.pyvimrc"

source "$DOTFILES_DIR/generic_rc_installer.sh"

doXremap(){
  if ! userHasSudo; then
    return
  fi
  echo "Setting up xremap rules"
  # Best installed with cargo I think.
  # https://github.com/k0kubun/xremap
  # Ubuntu setup rules
  echo uinput | sudo tee /etc/modules-load.d/uinput.conf
  sudo gpasswd -a $USER input
  echo 'KERNEL=="uinput", GROUP="input", TAG+="uaccess"' | sudo tee /etc/udev/rules.d/input.rules
  echo 'KERNEL=="event*", NAME="input/%k", MODE="660", GROUP="input"' | sudo tee /etc/udev/rules.d/input.rules
  sudo modprobe uinput
  sudo udevadm control --reload-rules && sudo udevadm trigger
}


installID="EspansoConf"
installText="extra_includes:\n  - \\\"$($SCRIPTDIR_CMD)/espanso/match/base.yml\\\"\n  - \\\"$($SCRIPTDIR_CMD)/espanso/config/default.yml\\\""
baseRC="${XDG_CONFIG_HOME}/espanso/config/default.yml"
source "$DOTFILES_DIR/generic_rc_installer.sh"
doEspanso(){
  doEspansoConf
  if command -v espanso > /dev/null 2>&1; then
    espanso service register
    espanso start
  fi
}

doMisc(){
  doVieb
  doPyvim
  doEspanso
  # doXremap
}

# If directly run instead of sourced, do all
if [ ! "${BASH_SOURCE[0]}" != "${0}" ]; then
  doMisc
fi
