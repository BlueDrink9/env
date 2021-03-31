#!/usr/bin/env bash
source "$DOTFILES_DIR/shell/script_functions.sh"
source "$DOTFILES_DIR/shell/functions.sh"
# To get access to `pack`
PATH="$DOTFILES_DIR/shell/scripts:$PATH"

installID="Packages"

eval "$(cat <<END
do${installID}() {
    printErr "Installing packages..."
    installPackages
}
END
)"

eval "$(cat <<END
undo${installID}(){
    uninstallPackages
}
END
)"

installList(){
    list="$1"
    while read -r package; do
        if [ "${package:0:1}" != "#" ]; then
            export HOMEBREW_NO_AUTO_UPDATE=1
            export PACK_NOCONFIRM=1
            pack install $package
        fi
    done < "$list"
}

installPackages(){
    # To get "pack" function
    source "$DOTFILES_DIR/shell/functions.sh"
    if grep -qE "(Microsoft|WSL)" "$([ -f /proc/version ] && cat /proc/version)" > /dev/null 2>&1; then
      export isWSL=1
    fi
    if sudo -v; then
      # Keep sudo alive until parent process dies.
      while true; do sleep 60; sudo -nv; kill -0 "$$" || exit; done 2>/dev/null &
    fi
    # Update
    pack refresh
    printLine "Installing base packages..."
    installList "$($SCRIPTDIR_CMD)/base/list"
    if [[ "$OSTYPE" =~ "darwin1" ]] || [ -n "$DISPLAY" -a -z "${isWSL}" ]; then
        printLine "Installing base gui packages..."
        installList "$($SCRIPTDIR_CMD)/base/guilist"
    fi
    if [[ "$OSTYPE" =~ "darwin1" ]]; then
        printLine "Installing OSX packages..."
        installList "$($SCRIPTDIR_CMD)/OSX/list"
        # Need xcode-8 cmd tools
    elif [[ "$(uname -a)" =~ "Android" ]]; then
        printLine "Installing termux packages..."
        installList "$($SCRIPTDIR_CMD)/termux/list"
        termux-setup-api
    elif [[ "$OSTYPE" =~ "linux" && -z "${isWSL}" ]]; then
        # Linux GUI stuff.
        printLine "Installing linux-specific packages..."
        installList "$($SCRIPTDIR_CMD)/linux/list"
    fi
}

uninstallPackages(){
    echo "Too hard to implement uninstalling packages"
}

installBrew() {
    if command -v brew >/dev/null 2>&1; then
        echo "Brew already installed"
        return
    fi
	if [[ "$OSTYPE" =~ 'darwin' ]]; then
		cd "$HOME" && mkdir -p homebrew && curl --insecure -L https://github.com/Homebrew/brew/tarball/master | tar xz --strip 1 -C homebrew
        export HOMEBREW_PREFIX="$HOME/homebrew"
	else
		sh -c "$(curl -fsSL https://raw.githubusercontent.com/Linuxbrew/install/master/install.sh)"
        export HOMEBREW_PREFIX="$HOME/.linuxbrew"
	fi
}

undoBrew(){
    rm -rf "$HOMEBREW_PREFIX"
    true #TODO
}

# If directly run instead of sourced, do all
if [ ! "${BASH_SOURCE[0]}" != "${0}" ]; then
    if askQuestionYN "Install and use brew for packages?"; then
        installBrew
    fi
    do${installID}
fi
