#!/usr/bin/env bash

# {[} Setup and variables
# For debugging use
# set -eEuxo pipefail
# set -uxo pipefail
SCRIPTDIR_CMD='eval echo $(cd $( dirname "${BASH_SOURCE[0]}" ) && pwd)'
SCRIPTDIR="$($SCRIPTDIR_CMD)"
# SCRIPTDIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
export DOTFILES_DIR="$SCRIPTDIR"
# SCRIPT COLORS are kept in this file
source "$SCRIPTDIR/bash/functions.sh"
source "$SCRIPTDIR/bash/script_functions.sh"
# if  compareVersionNum $BASH_VERSION_CLEAN '>' 4.2 ; then
source "$SCRIPTDIR/bash/colour_variables.sh"
OK="[ ${Green}OK${NC} ]"
Error="[ ${Red}ERROR${NC} ]"
ALL=0

source "$DOTFILES_DIR/bash/install.sh"
source "$DOTFILES_DIR/git/install.sh"
source "$DOTFILES_DIR/terminal/install.sh"
source "$DOTFILES_DIR/editors/vim/install.sh"
source "$DOTFILES_DIR/editors/vscode/install.sh"
source "$DOTFILES_DIR/system/install-brew.sh"
source "$DOTFILES_DIR/font_install.sh"
source "$DOTFILES_DIR/system/ssh/install.sh"

# {]} Setup and variables

uninstall() {
    if askQuestionYN "Really uninstall?"; then
        set_up="Remove custom"
        installStr="Uninstall"
        readSettings
        for installer in $installers; do
            un$installer
        done

        # Remove self
        if askQuestionYN "Delete repo directory?"; then
            rm -rf "$($SCRIPTDIR_CMD)"
        fi
        # Reset bash
        exec bash
    fi
}

installers=""

readSettings() {
    # May be set to "uninstall" rather than defaults set here.
    set_up="${set_up:-Set up}"
    installStr="${installStr:-Install}"
    # Order is designed so that things that may prompt for input
    # within installer are done first, and within those, the ones that may
    # take a while (eg downloading things) are last.
    # Put $installers after to bump something to the front of the queue.

    # Functions must be called "do[X]", with a corresponding "undo[X]" to uninstall.
    if [ $ALL != 1 ]; then
        if [ "$ALL" == 1 ] || askQuestionYN "$set_up shell?" ; then
            installers="$installers doShell"
        fi
        if [ "$ALL" == 1 ] || askQuestionYN "$set_up SSH?" ; then
            installers="$installers doSSH"
        fi
        if [ "$ALL" == 1 ] || askQuestionYN "$set_up vim?" ; then
            installers="$installers doVim"
        fi

        if [ "$ALL" == 1 ] || askQuestionYN "$set_up terminal?" ; then
            if substrInStr "kitty" "$TERM"; then
                installers="$installers doKitty"
            elif substrInStr "Android" "$(uname -a)";  then
                installers="$installers doTermux"
            fi
            installers="$installers doTmux"
            installers="$installers doX"

        fi

        if [ "$ALL" == 1 ] || askQuestionYN "$installStr fonts?" ; then
            installers="$installers doFonts"
        fi
        # if [ "$ALL" == 1 ] || askQuestionYN "Install VSCode extensions?" ; then
        #     installers="$installers vscodeExtensions"
        # fi

        if [ "$ALL" == 1 ] || askQuestionYN "$installStr brew?" ; then
            installers="$installers installBrew"
            if [ "$ALL" == 1 ] || askQuestionYN "Update brew and install \
                Brewfile packages? (This can take a very long time)" ; then
                installers="$installers updateBrew"
            fi
        elif [ "$ALL" == 1 ] || askQuestionYN "$installStr packages? (May require sudo)?" ; then
                installers="doPackages $installers"
        fi

        if [ "$ALL" == 1 ] || askQuestionYN "$set_up git credentials?" ; then
            installers="doGit $installers"
        fi
    fi
}

main() {
    readSettings
    for installer in $installers; do
        $installer ${1:-}
    done
    printErr "${Green} Install Complete${NC}"

    # Restart bash
    exec bash
}

# set default arg to avoid warnings
arg1=${1:-}
# echo 'arg' $arg1

if [[ $arg1 = "-u" ]]; then
    uninstall
    exit
fi

if [[ $arg1 =~ ^--?[aA][lL]{2}?$ ]]; then
    ALL=1
fi

if [[ $OSTYPE =~ 'linux' ]]; then

    printErr "[$Green Linux ${NC}]"
    main ${1:-}

elif [[ $OSTYPE =~ 'darwin' ]]; then
    printErr "[$Green OSX ${NC}]"
    main ${1:-}

elif [[ $OSTYPE == 'msys' ]]; then
    printErr "${Red}Git Bash not supported."
    if askQuestionYN "Continue anyway?" ; then
        printLine "${Red}Attempting install on Git Bash."
        main ${1:-}
    fi
else
    printErr "OS not detected..."
    if askQuestionYN "Continue anyway?" ; then
        main ${1:-}
    else
        printErr "Exiting without change."
    fi
fi
