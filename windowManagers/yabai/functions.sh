#!/usr/bin/env sh

requestUserText(){
  prompt="${1:-No prompt specified}"
  default="${2:-default}"
  echo $(osascript -e "text returned of (display dialog \"$prompt\" default answer \"$default\")")
  unset prompt
  unset default
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
