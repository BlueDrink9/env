# vim:ts=2:sw=2
# vim: foldmethod=marker
# vim: foldmarker={[},{]}
# This file holds functions for use in scripting

[ -n "${SCRIPT_FUNCTIONS_LOADED}" ] && return || SCRIPT_FUNCTIONS_LOADED=1

# For debugging use
# set -eEuxo pipefail
# set -uxo pipefail

SCRIPTDIR_CMD='eval echo $(cd $( dirname "${BASH_SOURCE[0]}" ) && pwd)'
# Usage:
# SCRIPTDIR="$($SCRIPTDIR_CMD)"

printLine() {
  # Replacing %s with %b seems to allow the escape sequences to pass on OSX...
  printf -- "%b\n" "$@"
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
    curl -fL "$url" 2> /dev/null
  else
    wget -qO- "$url" 2> /dev/null
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
  getWebItem "$url" >| "$filename"
}

getLatestReleaseFileURL() {
  # Takes argument 1 of form user/repo, eg will-shaw/env.
  # Gets the URL of the latest-released version of the specified filename arg 2.
  # example: saucecodeproURL=$(getLatestReleaseFileURL "ryanoasis/nerd-fonts" "SourceCodePro\.zip")
  default="invalid url or filename"
  repo=${1:-default}
  file=${2:-default}
  repo=$1
  file=$2
  repoapi=$(getWebItem "https://api.github.com/repos/${repo}/releases/latest")
  searchTemplate=https://github.com/${repo}/releases/download/[^/]*/${file}
  fileLatestURL=$(echo $repoapi | sed -n -e "s,^.*\(${searchTemplate}\).*$,\1,p")
  echo "$fileLatestURL"
}

downloadURLAndExtractZipTo() {
  downloadURLAndExtractTo zip "$@"
}
downloadURLAndExtractGzTo() {
  downloadURLAndExtractTo gz "$@"
}

downloadURLAndExtractTo() {
  # Two arguments: url, and destination folder.
  default="invalid url or filename"
  extension=${1:-default}
  url=${2:-default}
  destDir=${3:-default}
  urlFilename="${url##*/}"
  if [ "$url" = "default" ] || [ "$destDir" = "default" ]; then
    printErr "Error: Invalid url or dest"
  fi
  if [ ! -d "$destDir" ]; then
    mkdir -p "$destDir"
  fi
  tmpfile="$(mktemp).$extension"
  downloadURLtoFile "$url" "$tmpfile"
  set -x
  case "$extension" in
    zip)
      unzip -o "$tmpfile" -d "$destDir" # > /dev/null
      ;;
    *gz)
      # gzip doesn't have a way to specify the output dir.
      # Instead, we will name it the end of the url, minus extension.
      # Destdir includes slash, make note.
      local gunzipName="${urlFilename%.*}"
      tar -xkzf "$tmpfile" -C "${destDir}" || \
        gunzip -ck "$tmpfile" > "${destDir}"/"${gunzipName}"
      ;;
    bz2)
      tar -xkzjf "$tmpfile" -C "$destDir"
      ;;
    *)
      printErr "Invalid extension: \"$extension\""
  esac
  set +x
  if [ $? -eq 0 ]; then
    printErr "${Red}unzipped${NC}"
  else
    printErr "Error unzipping"
  fi
  rm -f "$tmpfile"
}

addTextIfAbsent() {
  default="invalid text or filename"
  text="${1:-$default}"
  file="${2:-$default}"
  mkdir -p "$(dirname "$file")"
  # Check if text exists in file, otherwise append.
  grep -q -F "$text" "$file" > /dev/null 2>&1 || echo "$text" >> "$file"
}

askQuestionYN() {
  default="?"
  question=${1:-default}
  echo -ne "${question} (y/n) " >&2
  read -n 1 REPLY
  printErr ""
  if [[ $REPLY =~ ^[yY]$ ]]; then
    return 0
  else
    return 1
  fi
}

resolveSymlinkToDir() {
  # doesn't work with directories. Would have to check if link is directory,
  # then not use pwd if it is (or append basename).
  link="$1"
  linkTarget="$(readlink "$link")"
  # If not a symlink, set to original.
  linkTarget="${linkTarget:-$link}"
  linkDir="$(dirname "$link")"
  targetDir="$(dirname "$linkTarget")"
  # cd first to link dir, in case link is relative.
  pushd "$linkDir" > /dev/null 2>&1
  pushd "$targetDir" > /dev/null 2>&1
  path="$(pwd -P)"
  echo "$path"
  popd > /dev/null 2>&1
  popd > /dev/null 2>&1
  unset link linkTarget targetDir linkDir path
}

userHasSudo(){
  prompt=$(sudo -nv 2>&1)
  if [ $? -eq 0 ]; then
    # exit code of sudo-command is 0
    return 0
  elif echo $prompt | grep -q '^sudo:'; then
    # echo "has_sudo__needs_pass"
    return 0
  else
    return 1
  fi
}
