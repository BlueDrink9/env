
# vim:ts=2:sw=2
# This file holds reusable functions

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

del() {
  # Safer than rm
  mv "$1" "${HOME}/.local/share/Trash/files/$1"
}

# Delete and reclone current git directory
reclone() {
  if is_git_repository ; then
    remoteUrl=`git config --get remote.origin.url`
    repoFolder=`pwd`
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
  elif [[  "$1" =~ "github.com" ]] ; then
    git clone https://$1
  else
    git clone https://github.com/$1
  fi
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
  BRANCH=`git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/\1/'`
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
  exec   >| >(tee -ia .debug_log_bashsetx)
  exec  2>| >(tee -ia .debug_log_bashsetx >& 2)
  exec 19>| .debug_log_bashsetx

  export BASH_XTRACEFD="19"

  # Fail on first error, output all commands, etc.
  set pipefail
  set -eEuxo
}
