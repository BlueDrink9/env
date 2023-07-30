#!/usr/bin/env bash
# Source these for required functions and XDG variables (if used)
source "$DOTFILES_DIR/shell/script_functions.sh"
source "$DOTFILES_DIR/shell/functions.sh"
source "$DOTFILES_DIR/shell/XDG_setup.sh"

installID="IPython"
installText="c.TerminalIPythonApp.extra_config_file = '$($SCRIPTDIR_CMD)/ipython.py'"
baseRC="${HOME}/.ipython/profile_default/ipython_config.py"

source "$DOTFILES_DIR/generic_rc_installer.sh"

installID="Pyvim"
# installText="with open(r'$DOTFILES_DIR/editors/pyvim/pyvimrc') as infile: exec(infile.read())"
installText="from importlib.machinery import SourceFileLoader\nconfigure = SourceFileLoader('mypyvimrc', '$DOTFILES_DIR/editors/pyvim/pyvimrc').load_module().configure"
baseRC="${HOME}/.pyvimrc"

source "$DOTFILES_DIR/generic_rc_installer.sh"

doXremap(){
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

# If directly run instead of sourced, do all
if [ ! "${BASH_SOURCE[0]}" != "${0}" ]; then
  do${installID}
  doXremap
fi
