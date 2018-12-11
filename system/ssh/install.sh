#!/usr/bin/env bash
source "$DOTFILES_DIR/bash/script_functions.sh"

doSSH() {
    printErr "Enabling SSH config..."
    # Note: Includes only added since 7.3p1
    addTextIfAbsent "Include \"$($SCRIPTDIR_CMD)/ssh_config\"" "${HOME}/.ssh/ssh_config"

    for key in $($SCRIPTDIR_CMD)/authorized_keys; do
        key="$(cat \"$key\")"
        addTextIfAbsent "$key" "${HOME}/.ssh/authorized_keys"
    done
}

undoSSH(){
    for key in $($SCRIPTDIR_CMD)/authorized_keys; do
        key="$(cat \"$key\")"
        sed -in "s|$key||g" "${HOME}/.ssh/authorized_keys"
    done
    sed -in "s|.*$($SCRIPTDIR_CMD)/ssh_config.*||g" "${HOME}/.ssh/ssh_config"
}

# If directly run instead of sourced, do all
if [ ! "${BASH_SOURCE[0]}" != "${0}" ]; then
    setupSSH
fi
