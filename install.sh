#!/usr/bin/env bash

# {[} Setup and variables
# For debugging use
# set -eEuxo pipefail
# set -uxo pipefail
SCRIPTDIR_CMD_BASE='$(cd $( dirname "${BASH_SOURCE[0]}" ) && pwd)'
SCRIPTDIR_CMD="eval echo $SCRIPTDIR_CMD_BASE"
SCRIPTDIR="$($SCRIPTDIR_CMD)"
export DOTFILES_DIR="$SCRIPTDIR"

source "$SCRIPTDIR/shell/functions.sh"
source "$SCRIPTDIR/shell/script_functions.sh"
# if  compareVersionNum $BASH_VERSION_CLEAN '>' 4.2 ; then
source "$SCRIPTDIR/shell/bash/colour_variables.sh"

if command -v setxkdmap > /dev/null 2>&1; then
  setxkbmap us,us -variant "colemak," && \
    printLine "${Green}Keyboard set to colemak."
fi
if substrInStr "darwin" "$OSTYPE"; then
  export XDG_CONFIG_HOME="$HOME/.config"
fi
# $XDG_CONFIG_HOME_DEFAULT="$HOME/.config"
export XDG_CONFIG_HOME="${XDG_CONFIG_HOME:-$HOME/.config}"
mkdir -p "$XDG_CONFIG_HOME"
echo "$DOTFILES_DIR" > "$XDG_CONFIG_HOME/.dotfiles_dir"
OK="[ ${Green}OK${NC} ]"
Error="[ ${Red}ERROR${NC} ]"
ALL=0
# Just does a quick setup of dots.
LITE=0

source "$DOTFILES_DIR/editors/vim/install.sh"
source "$DOTFILES_DIR/editors/emacs/install.sh"
source "$DOTFILES_DIR/font_install.sh"
source "$DOTFILES_DIR/git/install.sh"
source "$DOTFILES_DIR/shell/install.sh"
source "$DOTFILES_DIR/system/x11/install.sh"
source "$DOTFILES_DIR/system/OSX/install.sh"
source "$DOTFILES_DIR/system/packages/install.sh"
source "$DOTFILES_DIR/system/ssh/install.sh"
source "$DOTFILES_DIR/terminal/install.sh"
source "$DOTFILES_DIR/windowManagers/install.sh"
source "$DOTFILES_DIR/misc/install.sh"
# Need to change vscode's installer to not autorun. Or maybe leave to only run manually...
# source "$DOTFILES_DIR/editors/vscode/install.sh"

# # Source every `install.sh` file in env
# # Problematic because some of the installers are recursive.
# find "$DOTFILES_DIR" -name "install.sh" -print0 |
#   while read installer; do
#     source "$installer"
#   done

# {]} Setup and variables

uninstall() {
  if askQuestionYN "Really uninstall?"; then
    set_up="Remove custom"
    installStr="Uninstall"
    readSettings
    for installer in $installers; do
      un$installer
    done

    rm -f "$XDG_CONFIG_HOME/.dotfiles_dir"
    # Remove self
    if askQuestionYN "Delete repo directory?"; then
      rm -rf "$($SCRIPTDIR_CMD)"
    fi
    # Reset bash
    exec bash
  fi
}

installers=""

readSettings() {
  # May be set to "uninstall" rather than defaults set here.
  set_up="${set_up:-Set up}"
  installStr="${installStr:-Install}"
  # Order is designed so that things that may prompt for input
  # within installer are done first, and within those, the ones that may
  # take a while (eg downloading things) are last.
  # Put $installers after to bump something to the front of the queue.

    # Functions must be called "do[X]", with a corresponding "undo[X]" to uninstall.
    
    # Pretty much always want these, and most are fairly low-cost (mostly just dotfiles)
    installers="$installers doShell"
    installers="$installers doVim"
    installers="$installers doTmux"
    installers="$installers doGit"
    installers="$installers doEmacs"
    installers="$installers doMisc"

    # For android, use termux. For unix, use kitty.
    # (For Win, use Alacritty or Windows Terminal).
    if substrInStr "Android" "$(uname -a)";  then
      installers="$installers doTermux"
    elif [ "$OSTYPE" != "msys" ]; then
      installers="$installers doKitty"
    else
      installers="$installers doAlacritty"
    fi
    if substrInStr "darwin" "$OSTYPE"; then
      installers="$installers doiTerm2"
    fi


    if [ "$LITE" = 1 ]; then
      return
    fi

    # Asks for gitlab login for shared server repo, so put early.
    if [ "$ALL" = 1 ] || askQuestionYN "$set_up SSH server config from gitlab?" ; then
      installers="$installers doSSH"
    fi

    # if [ "$ALL" = 1 ] || askQuestionYN "$set_up emacs?" ; then
      # installers="$installers doEmacs"
    # fi

    # Install brew if OS X or no sudo. Otherwise use normal packages.
    if [ "$ALL" = 1 ] || askQuestionYN \
      "$installStr packages (May require sudo and take a while)?" ; then
      if substrInStr "darwin" "$OSTYPE" || ! sudo -v ; then
        installers="$installers installBrew"
      fi
      installers="$installers doPackages"
    fi

    if ! substrInStr "Android" "$(uname -a)" && [ "$OSTYPE" != "msys" ];  then
      if [ "$ALL" = 1 ] || askQuestionYN "Will this be a GUI system?" ; then
	installers="$installers doFonts"
	if [ "$OSTYPE" = "linux-gnu" ]; then
	  installers="$installers doXresources"
	  installers="$installers doX11"
	elif substrInStr "darwin" "$OSTYPE"; then
	  installers="$installers doOSX"
	fi
	installers="$installers doWM"
      fi
    fi

    # if [ "$ALL" = 1 ] || askQuestionYN "Install VSCode extensions?" ; then
    #     installers="$installers vscodeExtensions"
    # fi

  }

  main() {
    readSettings
    for installer in $installers; do
      "$installer" "${1:-}"
    done
    printErr "${Green} Install Complete${NC}"

    # Restart shell
    exec "$SHELL"
  }

# set default arg to avoid warnings
arg1=${1:-}
# echo 'arg' $arg1

if [ "$arg1" = "-u" ]; then
  uninstall
  exit
fi

if [[ $arg1 =~ ^--?[aA][lL]{2}?$ ]]; then
  ALL=1
fi

if [ "$arg1" = "-l" ]; then
  LITE=1
fi

if [ "$OSTYPE" = "linux-gnu" ]; then
  printErr "[$Green Linux ${NC}]"
elif substrInStr "darwin" "$OSTYPE"; then
  printErr "[$Green OSX ${NC}]"
elif [ "$OSTYPE" = "msys" ]; then
  printErr "[$Green Win (git bash) ${NC}]"
else
  printErr "OS not detected..."
  if askQuestionYN "Continue anyway?" ; then
    true
  else
    printErr "Exiting without change."
    exit
  fi
fi
main "${1:-}"
