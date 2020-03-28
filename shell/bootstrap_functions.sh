#!/bin/sh
# For bootstrapping (shell init) functions shared between bash, zsh, other
# posix shells.

extractPSCmdCol(){
  # The PS COMMAND col is sometimes abbreviated CMD
  awk -v p='C(OM)?M(AN)?D' 'NR==1 {n=match($0, p); next} {print substr($0, n)}'
}
getShellProgram(){
  # Get cmd of current process, last row of ps, strip leading - (osx) and trailing args.
  # Doesn't strip directory though...
  basename $(ps -p $$ | extractPSCmdCol | tail -1 | sed 's/^-//' | sed 's/ -.*//')
  # Get default shell, stripping leading '-' and directory
  # For some reason this just prints the function name.
  # shell_base="${0##*/}"
  # shell_base="${shell_base#-}"
  # echo $shell_base
  # unset $shell_base
}

loadProfile(){
  # When changing shells, force setup again.
  SHELL_PROGRAM="$(getShellProgram)"
  if [ "$SHELL_PROGRAM" != "$(basename "$SHELL")" ]; then
    unset PROFILE_LOADED
    export SHELL="$(which $SHELL_PROGRAM)"
  fi
  if [ -z "$PROFILE_LOADED" ]; then
    source "${SCRIPT_DIR}"/profile.*
  fi
}
