#!/bin/sh
# For bootstrapping (shell init) functions shared between bash, zsh, other
# posix shells.

extractPSCmdCol(){
  # The PS COMMAND col is sometimes abbreviated CMD
  awk -v p='C(OM)?M(AN)?D' 'NR==1 {n=match($0, p); next} {print substr($0, n)}'
}

getShellProgram(){
  # Get cmd of the current process, last row of ps, strip leading - (OSX), and trailing args
  ps -p $$ | extractPSCmdCol | tail -n1
  # Commented these bits because I don't seem to need them on linux -
  # might on OSX?
  # | {
  #   read line
  #   line=${line#-}     # Remove leading dash
  #   set -- $line       # Split into words
  #   echo "$1"          # Output the command part (first word)
  # }
}

loadProfile(){
  # When changing shells, force setup again.
  SHELL_PROGRAM="$(getShellProgram)"
  # $SHELL will retain the value of the login shell if starting a
  # new shell as a subprocess, so we can't rely on just using
  # $SHELL. ##*/ to get basename of process.
  if [ "$SHELL_PROGRAM" != "${SHELL##*/}" ]; then
    unset PROFILE_LOADED
    export SHELL="$(which $SHELL_PROGRAM)"
  fi
  if [ -z "$PROFILE_LOADED" ]; then
    source "${SCRIPT_DIR}"/profile.*
  fi
}
