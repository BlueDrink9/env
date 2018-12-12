# vim:ts=2:sw=2
# vim:foldmarker={[},{]}:foldmethod=marker
# This file holds reusable functions
[ ! -z ${BASH_FUNCTIONS_LOADED+} ] && return || export BASH_FUNCTIONS_LOADED=1

# Removes carriage return characters from argument file.
rmcr() {
  sed -i 's/\r$//' $1
}

# Replaces a file with the .bak version of itself.
mkbak() {
  cp $1 $1.bak
}

mkcd() {
  mkdir -p $1 && cd $1
}

sshraw() {
  ssh "$@" -t '/bin/bash --norc'
}

del() {
  fileToDel=$1
  fileDir=$(dirname ${fileToDel})
  if [[ $OSTYPE == 'linux-gnu' ]]; then
    TRASHDIR=${HOME}/.local/share/Trash/files
  elif [[ $OSTYPE =~ 'darwin' ]]; then
    TRASHDIR=${HOME}/.Trash
  fi
  mkdir -p "${TRASHDIR}/$fileDir"
  # Safer than rm
  mv "$fileToDel" "${TRASHDIR}/$fileToDel"
}

# Delete and reclone current git directory
reclone() {
  if is_git_repository ; then
    remoteUrl=$(git config --get remote.origin.url)
    repoFolder=$(pwd)
    cd .. && rm -rf ${repoFolder} && git clone ${remoteUrl} && cd ${repoFolder}
  else
    echo "Error: not a git repository ${remoteUrl}"
    return 1
  fi
}

# provides shortcuts for git cloning
git_clone() {
  if [[  "$1" =~ "https://github.com" ]] ; then
    git clone $1
  elif [[  "$1" =~ "@github.com" ]] ; then
    git clone $1
  elif [[  "$1" =~ "github.com" ]] ; then
    git clone https://$1
  else
    git clone https://github.com/$1
  fi
}

#TODO rceate function to try a range of terms...
term() {
  terminals="
  kitty --single-instance -d $(pwd)
  gnome-terminal
  xfce4-terminal
  terminal
  iterm2
  wsl.exe
  "
  tarray=($terminals)
  for term in "${tarray[@]}" ; do
    # if [ $(function_exists "$browser") ]; then
    function_exists "$term"
    command -v $term >/dev/null 2>&1 # deliberately only get first word of cmd
    if [ "$?" -eq 0 ]; then
      "$term" "$1"
      break
    fi
  done || echo "Terminal emulator unknown" >&2
}

fopen() {
  if [ "$(uname)" == "Darwin" ]; then
    # Mac OS X, open finder.
    open $1
    return
  fi
  filebrowsers="
  xdg-open
  explorer.exe
  finder
  nautilus
  gnome-open
  caja
  dolphin
  konquerer
  nemo
  "
  fbarray=($filebrowsers)
  for browser in "${fbarray[@]}" ; do
    # if [ $(function_exists "$browser") ]; then
    function_exists "$browser"
    command -v $browser >/dev/null 2>&1
    if [ "$?" -eq 0 ]; then
      $browser "$1"
      break
    fi
  done || echo "File browser unknown" >&2
}


# Save all commands with timestamp and working dir to log file. Doesn't
# affect bash's recallable history, or speed.
# https://spin.atomicobject.com/2016/05/28/log-bash-history/
log_command() {
  if [ "$(id -u)" -ne 0 ]; then
    echo "$(date "+%Y-%m-%d.%H:%M:%S") $(pwd) $(history 1)" >> ~/.logs/bash-history-$(date "+%Y-%m-%d").log;
  fi
}

randGen() {
  for ((i = 0; i < $1; i++)); do
    echo $RANDOM
  done
}

# Detect whether the current directory is a git repository.
is_git_repository() {
  git branch > /dev/null 2>&1
}

get_git_branch() {
  BRANCH=$(git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/\1/')
  echo "${BRANCH}"
}


# If a string list (separated by a space, " ") contains the 2nd argument.
# @param $1 the list variable, eg $list = "one two"
# @param $2 the word to check
contains() {
  if  [[ $1 =~ (^|[[:space:]])$2($|[[:space:]]) ]]; then
    return 0
  else
    return 1
  fi
}

function_exists() {
    FUNCTION_NAME=$1
    declare -F "$FUNCTION_NAME" > /dev/null 2>&1
    return $?
}


# https://unix.stackexchange.com/a/48116
_bash_history_sync() {
  # Consider using just -a if you want commands to only be available to new shells,
  # so currently running shells have their commands listed more recently.
  builtin history -a         #1
  HISTFILESIZE=$HISTSIZE     #2
  builtin history -c         #3
  builtin history -r         #4
}
history() {                  #5
  _bash_history_sync
  builtin history "$@"
}

# Log bash output into log given by argument (or into default log).
bash_debug_log() {
  if [ "$1" = "" ]; then
    logfile="${HOME}/.debug_log_bashsetx"
  else
    logfile="$1"
  fi
  exec   >| >(tee -ia $logfile)
  exec  2>| >(tee -ia $logfile >& 2)
  exec 19>| $logfile

  export BASH_XTRACEFD="19"

  # Fail on first error, output all commands, etc.
  set pipefail
  set -eEuxo
}

# Call from a local repo to open the repository on github/bitbucket in browser
# Modified version of https://github.com/zeke/ghwd
repo() {
	# Figure out github repo base URL
	local base_url
	base_url=$(git config --get remote.origin.url)
	base_url=${base_url%\.git} # remove .git from end of string

	# Fix git@github.com: URLs
	base_url=${base_url//git@github\.com:/https:\/\/github\.com\/}

	# Fix git://github.com URLS
	base_url=${base_url//git:\/\/github\.com/https:\/\/github\.com\/}

	# Fix git@bitbucket.org: URLs
	base_url=${base_url//git@bitbucket.org:/https:\/\/bitbucket\.org\/}

	# Fix git@gitlab.com: URLs
	base_url=${base_url//git@gitlab\.com:/https:\/\/gitlab\.com\/}

	# Validate that this folder is a git folder
	if ! git branch 2>/dev/null 1>&2 ; then
		echo "Not a git repo!"
		exit $?
	fi

	# Find current directory relative to .git parent
	full_path=$(pwd)
	git_base_path=$(cd "./$(git rev-parse --show-cdup)" || exit 1; pwd)
	relative_path=${full_path#$git_base_path} # remove leading git_base_path from working directory

	# If filename argument is present, append it
	if [ "$1" ]; then
		relative_path="$relative_path/$1"
	fi

	# Figure out current git branch
	# git_where=$(command git symbolic-ref -q HEAD || command git name-rev --name-only --no-undefined --always HEAD) 2>/dev/null
	git_where=$(command git name-rev --name-only --no-undefined --always HEAD) 2>/dev/null

	# Remove cruft from branchname
	branch=${git_where#refs\/heads\/}

	[[ $base_url == *bitbucket* ]] && tree="src" || tree="tree"
	url="$base_url/$tree/$branch$relative_path"


	echo "Calling $(type open) for $url"

	open "$url" &> /dev/null || (echo "Using $(type open) to open URL failed." && exit 1);
}

# Get colors in manual pages
man() {
	env \
		LESS_TERMCAP_mb="$(printf '\e[1;31m')" \
		LESS_TERMCAP_md="$(printf '\e[1;31m')" \
		LESS_TERMCAP_me="$(printf '\e[0m')" \
		LESS_TERMCAP_se="$(printf '\e[0m')" \
		LESS_TERMCAP_so="$(printf '\e[1;44;33m')" \
		LESS_TERMCAP_ue="$(printf '\e[0m')" \
		LESS_TERMCAP_us="$(printf '\e[1;32m')" \
		man "$@"
}

# Checks if the first arg is a substring of the second.
substrInStr(){
  substring=$1
  string=$2
  if [ "$substring" = "" ]; then
    echo "substring is blank!"
    return 255
  fi
  # if [ "$substring" = "$string" ]; then return 0; fi
  if [ "$string" = "" ]; then
    return 1 # false
  fi
  if [ -z "${string##*$substring*}" ]; then
    return 0 # true
  else
    return 1
  fi
}
substrTest(){
  failed=0
  if ! substrInStr "positive-middle" " this is positive-middlely the middle "; then failed=1; fi
  if ! substrInStr "positive-left" "positive-leftly this is left"; then failed=2; fi
  if ! substrInStr "positive-right" "this is right, positive-right"; then failed=3; fi
  if substrInStr "blank" ""; then failed=4; fi
  if substrInStr "negative" "a random string without the word"; then failed=5; fi
  # if ! substrInStr "" ""; then failed=6; fi

  if [[ "${failed}" != "0" ]]; then
    echo "substrInStr failed test $failed!"
  fi
}
# substrTest


# Compare two dot-separated version numbers.
# Usage: compareVersionNum num1 '>=' num2, eg:
# if  compareVersionNum $BASH_VERSION_CLEAN '>' 4.2 ; then
compareVersionNum () {
  op=$2
  num1=$1
  num2=$3
  if [[ $num1 == $num2 ]]
  then
    res="="
    return [[ $res =~ $op ]]
  fi

  local IFS=.
  local i ver1=($num1) ver2=($num2)
  # fill empty fields in ver1 with zeros
  for ((i=${#ver1[@]}; i<${#ver2[@]}; i++)); do
    ver1[i]=0
  done
  for ((i=0; i<${#ver1[@]}; i++)); do
    if [[ -z ${ver2[i]} ]]; then
      # fill empty fields in ver2 with zeros
      ver2[i]=0
    fi
    if ((10#${ver1[i]} > 10#${ver2[i]})); then
      res=">"
      break
    fi
    if ((10#${ver1[i]} < 10#${ver2[i]})); then
      res="<"
      break
    fi
  done

  # Check if result is in op (for >=). Poisix-compat.
  if substrInStr $res $op; then
    return 0
  else
    return 1
  fi
}

# {[} Exporting for ssh

generate_export_termoptions_cmd(){
  out=""
  for option in ${TERMOPTIONS[*]}; do
    out="${out} export ${option}=${!option}; "
  done
  echo "${out}"
}

ssh_with_options(){
  host=$1
  local EXPORT_TERMOPTIONS_CMD=$(generate_export_termoptions_cmd)
  # Calls default shell, stripping leading '-'
  \ssh -t "$host" "${EXPORT_TERMOPTIONS_CMD} " '${0#-} -l -s'
}
alias ssh="ssh_with_options"

set_tmux_termoptions(){
  for option in ${TERMOPTIONS[*]}; do
    # If attaching to a running tmux
    # session, we set a variable in tmux's global environment from
    # the containing shell. (This must be done before attaching to work!)
    # We then attach, and bash runs the refresh function.

      ## Set tmux environment
      if is_tmux_running; then
        # Check that we're in a session
        if [ ! -z "$TMUX" ]; then
          for option in ${TERMOPTIONS[*]}; do
            # Refresh termoption shell variables by parsing tmux's global environment
            local optval="$(\tmux show-environment -g ${option} 2>/dev/null)"
            export "${option}"="${optval##*=}"
            unset optval
          done
        else
          # Should this go after attaching tmux or before???
          tmux setenv -g "${option}" "${!option}"
        fi
      fi
    done
}

is_tmux_running(){
  # Check tmux is installed
  if command -v tmux>/dev/null; then
    # Check tmux has a session running
    if ! \tmux ls 2>&1 | grep -q "no server running"; then
      return 0 # true
    fi
  fi
  return 1 # false
}

tmux_with_options(){
  set_tmux_termoptions
  \tmux "$@"
}
alias tmux="tmux_with_options"
# {]} Exporting for ssh

reset_ssh_permissions(){
  chmod 700 $HOME/.ssh
  chmod 644 $HOME/.ssh/*
  chmod 600 $HOME/.ssh/*_rsa
  chmod 644 $HOME/.ssh/*.pub
  chmod go-w $HOME
  chown $USER $HOME/.ssh
  chown $USER $HOME/.ssh/*
}
