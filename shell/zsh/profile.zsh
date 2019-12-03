#!/usr/bin/env zsh
PROFILE_DIR=${0:a:h}


# Exec wipes out function definitions, so kill include guard for profile.
unset SHELL_FUNCTIONS_LOADED
source "${PROFILE_DIR}/functions.zsh"

source "${PROFILE_DIR}/../profile.sh"
