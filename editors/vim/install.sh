#!/usr/bin/env bash
source "$DOTFILES_DIR/shell/script_functions.sh"

doVimPlugins(){
    printErr "Installing vim plugins..."
    # Install Plug (plugin manager)
    if [[ $OSTYPE == 'msys' ]]; then
      downloadURLtoFile https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim "${HOME}/vimfiles/autoload/plug.vim"
    else
      downloadURLtoFile https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim "${HOME}/.vim/autoload/plug.vim"
    fi
    # This has the problem of making the caret disappear in WSL...
    vim -E +PlugInstall +qall
    # Recover missing cursor due to previous command
    # reset
}

undoVimPlugins(){
    rm -rf "${HOME}/.vim/plugins"
    rm -f "${HOME}/.vim/autoload/plug.vim"
}

doVim(){
    printErr "Enabling vim setup..."
    addTextIfAbsent "so $($SCRIPTDIR_CMD)/vimrc" "${HOME}/.vimrc"
    if [[ $OSTYPE == 'msys' ]]; then
      # git bash on windows, so lets also add a windows rc.
      # Using the vimfiles dir messes up creating vimfiles because it uses vimrc dir as home.
      addTextIfAbsent "so $(cygpath.exe -w "$($SCRIPTDIR_CMD)/vimrc")" "${HOME}/vimfiles/vimrc"
      addTextIfAbsent "so $(cygpath.exe -w "$($SCRIPTDIR_CMD)/vimrc")" "${HOME}/_vimrc"
      # addTextIfAbsent "so $(cygpath.exe -w "$($SCRIPTDIR_CMD)/vimrc")" "${HOME}/vimrc"
    fi
    addTextIfAbsent "so $($SCRIPTDIR_CMD)/nvimrc" "${HOME}/.config/nvim/init.vim"
    doVimPlugins
    addTextIfAbsent "so $(realpath "$(${SCRIPTDIR_CMD})/../ideavimrc")" "${HOME}/.ideavimrc"
    printErr "Done setting up vim"
    if grep -qE "(Microsoft|WSL)" "$([ -f /proc/version ] && cat /proc/version)" > /dev/null 2>&1; then
      export isWSL=1
    fi
    if [ -n "${isWSL}" ]; then
      $NEOVIM_WIN_DIR="/mnt/c/tools/neovim/Neovim"
      sudo ln -s "$NEOVIM_WIN_DIR/bin/win32yank.exe" "/usr/local/bin/win32yank"
    fi
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
