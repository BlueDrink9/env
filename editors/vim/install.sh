#!/usr/bin/env bash
SCRIPTDIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

doVimPlugins(){
    printErr "Installing vim plugins..."
    # Install Plug (plugin manager)
    downloadURLtoFile https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim "${HOME}/.vim/autoload/plug.vim"
    # This has the problem of making the caret disappear in WSL...
    vim -E +PlugInstall +qall
    # Recover missing cursor due to previous command
    reset
}

undoVimPlugins(){
    rm -rf "${HOME}/vimfiles"
    rm -f "${HOME}/.vim/autoload/plug.vim"
}

doVim(){
    printErr "Enabling vim setup..."
    addTextIfAbsent "so $SCRIPTDIR/vimrc" "${HOME}/.vimrc"
    addTextIfAbsent "so $SCRIPTDIR/nvimrc" "${HOME}/config/nvim/init.vim"
    doVimPlugins
    printErr "Done setting up vim"
}

undoVim(){
    # The sed commands replace source lines with blanks
    rm -rf "${HOME}/.config/nvim"
    sed -in "s|.*${SCRIPTDIR}/vimrc.*||g" "${HOME}/.vimrc"
    sed -in "s|.*${SCRIPTDIR}/nvimrc.*||g" "${HOME}/.config/nvim/init.vim"
    undoVimPlugins
}

# If interactive, do all
if [[ $- == *i* ]]; then
    setupVim
fi
