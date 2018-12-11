#!/usr/bin/env bash
SCRIPTDIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

installBrew() {
	if [[ $OSTYPE =~ 'darwin' ]]; then
		cd "$HOME" && mkdir -p homebrew && curl --insecure -L https://github.com/Homebrew/brew/tarball/master | tar xz --strip 1 -C homebrew
        HOMEBREW_PREFIX="$HOME/homebrew"
	else
		sh -c "$(curl -fsSL https://raw.githubusercontent.com/Linuxbrew/install/master/install.sh)"
        HOMEBREW_PREFIX="$HOME/.linuxbrew"
	fi
}

updateBrew() {
    brew="$HOMEBREW_PREFIX/bin/brew"
    "$brew" update
    # This reads words. Currently ok, but should be updated. TODO
    for i in $(cat "$SCRIPTDIR/Brewfile"); do "$brew" install "$i"; done
}

undoBrew(){
    rm -rf "$HOMEBREW_PREFIX"
    true #TODO
}

# If interactive, do both
if [[ $- == *i* ]]; then
    installBrew
    updateBrew
fi
