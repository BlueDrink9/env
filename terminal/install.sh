#!/usr/bin/env bash
source "$DOTFILES_DIR/bash/script_functions.sh"

doTmux(){
    printErr "Enabling custom tmux setup..."
    addTextIfAbsent "source-file $($SCRIPTDIR_CMD)/tmux/tmux.conf" ${HOME}/.tmux.conf
}

doKitty() {
    printErr "Enabling Kitty setup..."
    addTextIfAbsent "include $($SCRIPTDIR_CMD)/kitty/kitty.conf" "${HOME}/.config/kitty/kitty.conf"
}

doTermux() {
    printErr "Enabling Termux setup..."
    mkdir -p "$HOME/.termux"
    downloadURLAndExtractZipTo "https://github.com/adi1090x/termux-style/raw/master/data.tar.gz" "$HOME/.termux/termux-style"
    cp "$HOME/.termux/termux-style/solarized-light.properties" "$HOME/.termux/"
}

doX() {
    printErr "Enabling custom X setup..."
    addTextIfAbsent "xrdb -merge \"$($SCRIPTDIR_CMD)/x/Xresources\"" "${HOME}/.Xresources"
}

undoTermux(){
    rm -rf "$HOME/.termux"
}
undoTmux(){
    sed -in "s|.*$($SCRIPTDIR_CMD)/tmux/tmux\\.conf.*||g" "${HOME}/.tmux.conf"
}
undoX(){
    sed -in "s|.*$($SCRIPTDIR_CMD)/x/Xresources.*||g" "${HOME}/.Xresources"
}
undoKitty(){
    sed -in "s|.*$($SCRIPTDIR_CMD)/kitty/kitty.conf.*||g" "${HOME}/.config/kitty/kitty.conf"
}

# If interactive, do all
if [[ $- == *i* ]]; then
    setupTmux
    if substrInStr "kitty" "$TERM"; then
        setupKitty
    elif substrInStr "Android" "$(uname -a)"; then
        setupTermux
    fi
    setupX
fi
