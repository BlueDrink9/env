#!/bin/bash
SCRIPTDIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

printLine() {
    printf -- "$@\n" 
}
printErr() {
    >&2 printLine "$@"
}
getWebItem() {
    default="no-url-given"
    url=${1:-default}
    printErr "Downloading $url ... "
    # OSX has curl by default, linux has wget by default
    # if [[ $OSTYPE =~ 'darwin' ]]; then
    if hash curl 2> /dev/null; then
        curl -fL $url 2> /dev/null
    else
        wget -qO- $url 2> /dev/null
    fi
    printErr "Done"
}

downloadURLtoFile() {
    default="invalid url or filename"
    url=${1:-default}
    filename=${2:-default}
    if [ "$url" = "default" ] || [ "$filename" = "default" ]; then
        printErr "Error: Invalid url or filename"
    fi
    downloadDirectory=$(dirname "$filename")
    if [ ! -d "$downloadDirectory" ]; then
        mkdir -p "$downloadDirectory"
    fi
    getWebItem $url >| $filename
}


tmprc=$(mktemp)
plugPattern='/^\s*[^\"]\s*Plug\s/ {print}'
plugPattern='/Plug/ {print}'
echo "call plug#begin(\"$HOME/vimfiles/plugins\")" >> $tmprc
awk "$plugPattern" $SCRIPTDIR/light_plugins.vim >> $tmprc
awk "$plugPattern" $SCRIPTDIR/main_plugins.vim >> $tmprc
awk "$plugPattern" $SCRIPTDIR/ide_plugins.vim >> $tmprc
echo "call plug#end()" >> $tmprc
# cat $tmprc

printErr "Installing vim plugins..."
# Install Plug (plugin manager)
downloadURLtoFile https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim "${HOME}/.vim/autoload/plug.vim"
# This has the problem of making the caret disappear in WSL...
vim -u $tmprc -E +PlugInstall +qall
# Recover missing cursor due to previous command
reset
rm $tmprc
