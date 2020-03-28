#!/bin/sh
# For bootstrapping (shell init) functions shared between bash, zsh, other
# posix shells.

extractPSCmdCol(){
  # The PS COMMAND col is sometimes abbreviated CMD
  awk -v p='C(OM)?M(AN)?D' 'NR==1 {n=match($0, p); next} {print substr($0, n)}'
}
getShellProgram(){
  # Get cmd of current process, last row of ps, strip leading - (osx) and trailing args.
  ps -p $$ | extractPSCmdCol | tail -1 | sed 's/^-//' | sed 's/ -.*//'
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
