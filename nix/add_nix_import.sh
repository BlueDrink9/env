#!/bin/sh
source "$DOTFILES_DIR/shell/script_functions.sh"

AddNixImport(){
    importFile=$1
    baseFile=$2
    lineNumber=$(awk '/imports =/ {print NR; exit}' "$baseFile")
    lineNumber=$(($lineNumber+2))
    SudoFunction prependTextIfAbsent "${importFile}" "${baseFile}" $lineNumber
}

