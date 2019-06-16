#!/usr/bin/env sh

requestUserText(){
  prompt="${1:-No prompt specified}"
  default="${2:-default}"
  echo $(osascript -e "text returned of (display dialog \"$prompt\" default answer \"$default\")")
  unset prompt
  unset default
}
chunkwm_serialize(){
  layoutDir="$HOME/.config/chunkwm/layouts/"
  mkdir -p "$layoutDir"
  layout=$(requestUserText "Name of layout?" "default")
  chunkc tiling::desktop --serialize "$layoutDir/$layout"
  unset layoutDir
  unset layout
}

requestFile(){
  defaultDir="${1:-$HOME}"
  osascript <<-EndOfScript
set theFile to choose file with prompt "Please choose a file:" default location "$defaultDir"
set theFilePosix to (the POSIX path of theFile)
theFilePosix
EndOfScript
unset defaultDir
}
chunkwm_deserialize(){
  layoutDir="$HOME/.config/chunkwm/layouts/"
  mkdir -p "$layoutDir"
  layoutFile=$(requestFile "$layoutDir")
  chunkc tiling::desktop --deserialize "$layoutFile"
  unset layoutDir
  unset layoutFile
}
