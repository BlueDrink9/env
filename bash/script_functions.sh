# vim:ts=2:sw=2
# vim: foldmethod=marker
# vim: foldmarker={[},{]}
# This file holds functions for use in scripting

[ -n "$SCRIPT_FUNCTIONS_LOADED" ] && return || export SCRIPT_FUNCTIONS_LOADED=1

# For debugging use
# set -eEuxo pipefail
# set -uxo pipefail

SCRIPTDIR_CMD='eval echo $(cd $( dirname "${BASH_SOURCE[0]}" ) && pwd)'
# Usage:
# SCRIPTDIR="$($SCRIPTDIR_CMD)"

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
    # Two arguments: url, and destination folder.
    default="invalid url or filename"
    url=${1:-default}
    destDir=${2:-default}
    if [ "$url" = "default" ] || [ "$destDir" = "default" ]; then
        printErr "Error: Invalid url or dest"
    fi
    if [ ! -d "$destDir" ]; then
        mkdir -p $destDir
    fi
    tmpzipfile=$(mktemp)
    downloadURLtoFile $url $tmpzipfile.zip
    unzip -o $tmpzipfile.zip -d "$destDir" # > /dev/null
    rm -f $tmpzipfile.zip
}

addTextIfAbsent() {
    default="invalid text or filename"
    text=${1:-$default}
    file=${2:-$default}
    mkdir -p "$(dirname "$file")"
    # Check if text exists in file, otherwise append.
    grep -q -F "$text" "$file" || echo "$text" >> "$file"
}

printLine() {
    printf -- "%s\n" "$@"
}
printErr() {
    >&2 printLine "$@"
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
