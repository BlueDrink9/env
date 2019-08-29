#!/usr/bin/env bash
source "$DOTFILES_DIR/bash/script_functions.sh"

doVimPlugins(){
    printErr "Installing vim plugins..."
    # Install Plug (plugin manager)
    downloadURLtoFile https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim "${HOME}/.vim/autoload/plug.vim"
    # This has the problem of making the caret disappear in WSL...
    vim -E +PlugInstall +qall
    # Recover missing cursor due to previous command
    # reset
}

undoVimPlugins(){
    rm -rf "${HOME}/vimfiles"
    rm -f "${HOME}/.vim/autoload/plug.vim"
}

doVim(){
    printErr "Enabling vim setup..."
    addTextIfAbsent "so $($SCRIPTDIR_CMD)/vimrc" "${HOME}/.vimrc"
    addTextIfAbsent "so $($SCRIPTDIR_CMD)/nvimrc" "${HOME}/.config/nvim/init.vim"
    doVimPlugins
    addTextIfAbsent "so $(realpath "$(${SCRIPTDIR_CMD})/../ideavimrc")" "${HOME}/.ideavimrc"
    printErr "Done setting up vim"
}

undoVim(){
    # The sed commands replace source lines with blanks
    rm -rf "${HOME}/.config/nvim"
    sed -in "s|.*$($SCRIPTDIR_CMD)/vimrc.*||g" "${HOME}/.vimrc"
    sed -in "s|.*$($SCRIPTDIR_CMD)/nvimrc.*||g" "${HOME}/.config/nvim/init.vim"
    for i in ~/fzf*; do rm -rf "${HOME:?}/$i"; done
    rm -rf ~/ctrlpCache
    undoVimPlugins
}

# If directly run instead of sourced, do all
if [ ! "${BASH_SOURCE[0]}" != "${0}" ]; then
    doVim
fi
