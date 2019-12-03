#!/usr/bin/env zsh
PROFILE_DIR=${0:a:h}

source "${PROFILE_DIR}/../profile.sh"

# Exec wipes out function definitions, so kill include guard for profile.
unset SHELL_FUNCTIONS_LOADED
source "${PROFILE_DIR}/functions.zsh"
