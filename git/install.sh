#!/usr/bin/env bash
source "$DOTFILES_DIR/bash/script_functions.sh"

doGit() {
    gitSettings
    gitCredentialCache
    gitUser "$@"
}

undoGit() {
    if askQuestionYN "Remove stored git username and email?"; then
        git config --global --unset-all user.name
        git config --global --unset-all credential.https://github.com.username
        git config --global --unset-all user.email
    fi
    git config --global --unset core.excludesFile
    git config --global --unset core.attributesFile
    git config --global --unset include.path
}

gitUser() {
    if [[ ${1:-} =~ ^--?[gG](it)?-?[cC](redentials)? ]]; then
        printErr "Forcing git update."
    elif [[ "$(git config --global user.name)" =~ .+ ]] && [[ $(git config --global user.email) =~ .+ ]]; then
        printErr "$OK Git global user is set."
        printErr "Re-run with ( ./install -gu ) to force update."
        return 0
    fi

    printErr "Setting up git global user..."
    echo -ne "${Green}Enter your github username:${NC} "
    read -r GIT_USER
    echo -ne "${Green}Enter your git email:${NC} "
    read -r GIT_EMAIL

    printErr "${Yellow}Configuring git.$NC"
    printErr "Username: $GIT_USER"
    printErr "Email: $GIT_EMAIL"
    git config --global user.name "$GIT_USER"
    git config --global credential.https://github.com.username "$GIT_USER"
    git config --global user.email "$GIT_EMAIL"
    printErr "${Cyan}You can update your git user by entering:$NC ./install -gu"
}

gitCredentialCache() {
    if [ "$ALL" == 1 ] || askQuestionYN "${Yellow}Want Git to store your credentials for a while?${NC}"; then

        if [[ ! "$(git config --global user.name)" =~ .+ ]] || [[ ! $(git config --global user.email) =~ .+ ]]; then
            printErr "$OK Git global user is not setup correctly."
            printErr "Re-run with ( ./install -gu ) to force update."
            return 0
        fi

        printErr "${Green}How long should Git store your credentials? (minutes)${NC} "
        while true; do
            read -r REPLY
            if [[ $REPLY =~ ^[0-9]+$ ]] && [[ $REPLY -gt 0 ]]; then
                break
            else
                printErr "${Red}Invalid input${NC}"
            fi
        done
        printErr "Git will remember your credentials for $REPLY minutes ($(( REPLY * 60 )) seconds)."
        git config --global credential.helper "cache --timeout=$(( REPLY * 60 ))"
    fi
}

gitSettings() {
    printErr "Enabling custom git setup..."
    # Include is only supported on git versions > 1.7.10
    # (but 2.0 is quite standard anyway).
    # TODO check this, and also append contents if not.
    git config --global include.path "$($SCRIPTDIR_CMD)/gitconfig"
    git config --global core.excludesfile "$($SCRIPTDIR_CMD)/gitignore"
    git config --global core.attributesfile "$($SCRIPTDIR_CMD)/gitattributes"
}

# If directly run instead of sourced, do all
if [ ! "${BASH_SOURCE[0]}" != "${0}" ]; then
    source "$DOTFILES_DIR/bash/colour_variables.sh"
    doGit "$@"
fi
