#!/usr/bin/env bash
source "$DOTFILES_DIR/shell/script_functions.sh"
installID="SSH"
installText="Include \"$($SCRIPTDIR_CMD)/config\""
baseRC="${HOME}/.ssh/config"

eval "$(cat <<END
do${installID}() {
    printErr "Enabling SSH config..."
    # Note: Includes only added since 7.3p1
    addTextIfAbsent "Host *
    ${installText}" "${baseRC}"

    modify_SSH_env_keys
    if [ ! -d "$HOME/.ssh/shared_servers" ]; then
      git clone https://gitlab.com/BlueDrink9/shared-ssh-servers.git $HOME/.ssh/shared_servers
    fi
}
END
)"
modify_SSH_env_keys(){
    if [ "$1" = "-u" ]; then
        local remove=1
    fi
    scriptdir="$($SCRIPTDIR_CMD)"
    currDir="$(pwd)"
    cd ${scriptdir}/authorized_keys/
    for keyfile in *; do
        key="$(cat $keyfile)"
        if [ -z "$remove" ]; then
            addTextIfAbsent "$key" "${HOME}/.ssh/authorized_keys"
        else
            sed -in "s|$key||g" "${HOME}/.ssh/authorized_keys"
        fi
    done
    cd "${currDir}"
}


eval "$(cat <<END
undo${installID}(){
    sed -in "s|.*${installText}.*||g" "${baseRC}"
    modify_SSH_env_keys -u
}
END
)"

# If directly run instead of sourced, do all
if [ ! "${BASH_SOURCE[0]}" != "${0}" ]; then
    do${installID}
fi
set +x
