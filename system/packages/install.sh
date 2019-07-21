#!/usr/bin/env bash
source "$DOTFILES_DIR/bash/script_functions.sh"

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
            pack install $package
        fi
    done < "$list"
}

installPackages(){
    # To get "pack" function
    source "$DOTFILES_DIR/bash/functions.sh"
    # Update
    pack update
    installList "$($SCRIPTDIR_CMD)/base"
    if [[ $OSTYPE =~ "darwin1" ]]; then
        installList "$($SCRIPTDIR_CMD)/osx"
    elif [[ $OSTYPE =~ "linux" ]]; then
        installList "$($SCRIPTDIR_CMD)/linux"
    fi
    if [[ "$(uname -a)" =~ "Android" ]]; then
        installList "$($SCRIPTDIR_CMD)/termux"
         termux-setup-api
    fi
}

uninstallPackages(){
    echo "Too hard to implement uninstalling packages"
}

installBrew() {
    if [ $(command -v brew 2>/dev/null) ]; then
        echo "Brew already installed"
        return
    fi
	if [[ $OSTYPE =~ 'darwin' ]]; then
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
