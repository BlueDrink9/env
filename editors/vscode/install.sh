#!/usr/bin/env bash
source "$DOTFILES_DIR/bash/script_functions.sh"

VSCODE_EXTENSIONS_DIR="${HOME}/.vscode/extensions"
VSCODE_VERSION=code
VSCODE_APP_DATA="${HOME}/AppData/Roaming/Code"

# This is terribly written and does not work any more. Here for legacy/when I
# eventually decide to use VSCODE again (ie when neovim integration for it is good)
vscodeExtensions() {
    if hash code-insiders 2> /dev/null; then # Maybe insider version is being used.
        VSCODE_VERSION=code-insiders
        VSCODE_APP_DATA="${HOME}/AppData/Roaming/Code - Insiders/"
        VSCODE_EXTENSIONS_DIR="${HOME}/.vscode-insiders/extensions"
    fi
    if hash code 2> /dev/null || hash code-insiders 2> /dev/null; then # Check if 'code' exists.
        mkdir -p "$VSCODE_EXTENSIONS_DIR"
        while IFS='' read -r LINE || [[ -n "$LINE" ]]; do
            code --install-extension "$LINE"
        done < "$($SCRIPTDIR_CMD)/extensions"

        # elif [[ $REPLY =~ ^[cC]$ ]]; then # Load VSCode which detects recommendations.json
        #     #TODO Where is this meant to be CDed to?
        #     code ./editors
    else
        printErr "VSCode not in PATH"
        return 1
    fi

    if hash code 2> /dev/null; then # Check if 'code' exists.
        VSCODE_EXTENSIONS_DIR="${HOME}/.vscode/extensions"
    else
        echo -e "VSCode not installed or variable not set."
        return 1
    fi

    if [[ $REPLY =~ ^[yY]$ ]]; then # Install extensions from 'vscode/extensions'
        mkdir -p "$VSCODE_EXTENSIONS_DIR"
        mkdir -p "${VSCODE_APP_DATA}/User"

        while IFS='' read -r LINE || [[ -n "$LINE" ]]; do
            "${VSCODE_VERSION}" --install-extension "$LINE"
        done < "$($SCRIPTDIR_CMD)/extensions"

        cp "$($SCRIPTDIR_CMD)/settings.json" "${VSCODE_APP_DATA}/User"

    elif [[ $REPLY =~ ^[cC]$ ]]; then # Load VSCode which detects recommendations.json
        $VSCODE_VERSION ./editors
    fi
    return 0
}


# If interactive, do all
if [[ $- == *i* ]]; then
    vscodeExtensions
fi
