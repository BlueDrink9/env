#!/usr/bin/env bash
source "$DOTFILES_DIR/shell/script_functions.sh"
if [[ $OSTYPE == 'msys' ]]; then
  VIMFILES="${HOME}/vimfiles"
else
  VIMFILES="${HOME}/.vim"
fi

doVimPlugins(){
    printErr "Installing vim plugins..."
    # Install Plug (plugin manager)
    if [[ $OSTYPE == 'msys' ]]; then
      downloadURLtoFile https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim "${VIMFILES}/autoload/plug.vim"
    else
      downloadURLtoFile https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim "${VIMFILES}/autoload/plug.vim"
    fi
    # Do twice in case of segfaults. Install is idempotent, restore isn't
    # too slow.
    nvim --headless --cmd "let g:ideMode=1" "+Lazy! install | Lazy! restore" +qa
    nvim --headless --cmd "let g:ideMode=1" "+Lazy! install | Lazy! restore" +qa
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
    addTextIfAbsent "so $(realpath "$(${SCRIPTDIR_CMD})/../ideavimrc")" "${HOME}/.ideavimrc"
    # Nvim:
    addTextIfAbsent "dofile(\"$($SCRIPTDIR_CMD)/nvimrc.lua\")" "${HOME}/.config/nvim/init.lua"
    if [[ $OSTYPE == 'msys' ]]; then
      scriptDir="$(cygpath -m $($SCRIPTDIR_CMD))"
      # git bash on windows, so lets also add a windows rc.
      # Using the vimfiles dir messes up creating vimfiles because it uses vimrc dir as home.
      addTextIfAbsent "so $scriptDir/vimrc" "${VIMFILES}/vimrc"
      winNvimDir="${HOME}/AppData/Local/nvim/"
      mkdir -p "${winNvimDir}"
      addTextIfAbsent "$(printf 'dofile("%s/nvimrc.lua")' "$scriptDir")" "${winNvimDir}/init.lua"
    fi

    doVimPlugins
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
