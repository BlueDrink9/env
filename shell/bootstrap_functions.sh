#!/bin/sh
# For bootstrapping (shell init) functions shared between bash, zsh, other
# posix shells.

loadProfile(){
  # When changing shells, force setup again with preferred shell.
  SHELL_PROGRAM="$(getShellProgram)"
  # ##*/ to get basename of process.
  if [ "$SHELL_PROGRAM" != "${SHELL##*/}" ]; then
    unset PROFILE_LOADED
    export SHELL="$(which $SHELL_PROGRAM)"
  fi
  if [ -z "$PROFILE_LOADED" ]; then
    source "${SCRIPT_DIR}"/profile.*
  fi
}

getShellProgram() {
  # 1. Capture the data line (skipping the header)
  # 'read' without -r is fine here because we want word splitting anyway
  _line=$(ps -p $$ | {
    # Header line
    read -r _h
    # Data line
    read -r _l
    printf '%s' "$_l";
  })
  # 2. Trim trailing whitespace first (safety check)
  _line="${_line%"${_line##*[![:space:]]}"}"
  # 3. Strip everything up to the last space
  _cmd="${_line##* }"
  # 4. Cleanup for shell-specific quirks
  _cmd="${_cmd#-}"    # Remove leading dash (macOS/Login shells)
  _cmd="${_cmd##*/}"  # Remove path (e.g., /bin/sh -> sh)
  printf '%s\n' "$_cmd"
}
