#!/usr/bin/env bash
SCRIPTDIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

doSSH() {
    printErr "Enabling SSH config..."
    # Note: Includes only added since 7.3p1
    addTextIfAbsent "Include \"$SCRIPTDIR/ssh_config\"" ${HOME}/.ssh/ssh_config

    for key in ${SCRIPTDIR}/authorized_keys; do
        key="$(cat \"$key\")"
        addTextIfAbsent "$key" "${HOME}/.ssh/authorized_keys"
    done
}

undoSSH(){
    for key in ${SCRIPTDIR}/authorized_keys; do
        key="$(cat \"$key\")"
        sed -in "s|$key||g" "${HOME}/.ssh/authorized_keys"
    done
    sed -in "s|.*${SCRIPTDIR}/ssh_config.*||g" "${HOME}/.ssh/ssh_config"
}

# If interactive, do all
if [[ $- == *i* ]]; then
    setupSSH
fi
