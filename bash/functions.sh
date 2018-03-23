# vim: set ft=sh:
# vim:ts=2:sw=2

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

# Return the prompt symbol ($) to use, colorized based on the return value of the
# previous command.
set_prompt_symbol () {
  if test $1 -eq 0 ; then
    PROMPT_SYMBOL="${Green}\\$ ${NC}";
    PREV_COMMAND_COLOUR="${Green}";
  else
    PROMPT_SYMBOL="${Red}\\$ ${NC}";
    PREV_COMMAND_COLOUR="${Red}";
  fi
}

# Detect whether the current directory is a git repository.
is_git_repository() {
  git branch > /dev/null 2>&1
}

get_git_branch() {
  BRANCH=`git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/\1/'`
  echo "${BRANCH}"
}

# get current branch in git repo
# Check status of branch
# Green if no changes, yellow if modified. White if there are changes to files.
parse_git_branch() {
  STATUS_COLOUR=${NC}
  BRANCH=`get_git_branch`
  if [ ! "${BRANCH}" == "" ]
  then
    status=`git status 2>&1 | tee`
    dirty=`echo -n "${status}" 2> /dev/null | grep "modified:" &> /dev/null; echo "$?"`
    untracked=`echo -n "${status}" 2> /dev/null | grep "Untracked files" &> /dev/null; echo "$?"`
    ahead=`echo -n "${status}" 2> /dev/null | grep "Your branch is ahead of" &> /dev/null; echo "$?"`
    newfile=`echo -n "${status}" 2> /dev/null | grep "new file:" &> /dev/null; echo "$?"`
    renamed=`echo -n "${status}" 2> /dev/null | grep "renamed:" &> /dev/null; echo "$?"`
    deleted=`echo -n "${status}" 2> /dev/null | grep "deleted:" &> /dev/null; echo "$?"`
    bits=''
    if [ "${ahead}" == "0" ]; then
      STATUS_COLOUR=${Cyan}
      bits="^${bits}"
    fi
    if [ "${renamed}" == "0" ]; then
      bits=">${bits}"
    fi
    if [ "${untracked}" == "0" ]; then
      bits="?${bits}"
    fi
    if [ "${deleted}" == "0" ]; then
      bits="X${bits}"
    fi
    if [ "${newfile}" == "0" ]; then
      bits="+${bits}"
    fi
    if [ "${dirty}" == "0" ]; then
      STATUS_COLOUR=${Yellow}
      bits="*${bits}"
    fi
    if [ ! "${bits}" == "" ]; then
      STATUS="${bits}"
    else
      STATUS=""
      STATUS_COLOUR=${Green}
    fi

    # Sometimes status can be slow. Consider removing.
    GIT_PROMPT="${STATUS_COLOUR}{${BRANCH}${STATUS}}${NC}"
  else
    GIT_PROMPT=""
  fi
}



# Determine the branch/state information for this git repository.
set_git_branch(){
  # Capture the output of the "git status" command.
  git_status="$(git status 2> /dev/null)"

  # Set color based on clean/staged/dirty.
  if [[ ${git_status} =~ "working directory clean" ]]; then
    state="${BGreen}"
  elif [[ ${git_status} =~ "Changes to be committed" ]]; then
    state="${Yellow}"
  else
    state="${Red}"
  fi

  # Set arrow icon based on status against remote.
  remote_pattern="# Your branch is (.*) of"
  if [[ ${git_status} =~ ${remote_pattern} ]]; then
    if [[ ${BASH_REMATCH[1]} == "ahead" ]]; then
      remote="^"
    else
      remote="v"
    fi
  else
    remote="diverged"
  fi
  diverge_pattern="# Your branch and (.*) have diverged"
  if [[ ${git_status} =~ ${diverge_pattern} ]]; then
    remote=""
  fi

  # Get the name of the branch.
  branch_pattern="^# On branch ([^${IFS}]*)"
  if [[ ${git_status} =~ ${branch_pattern} ]]; then
    branch=${BASH_REMATCH[1]}
  fi

  # Set the final branch string.
  BRANCH="${state}(${branch})${remote}${NC} "
}

# Set the BRANCH variable.
set_git_prompt () {
  if is_git_repository ; then
    set_git_branch
  else
    BRANCH=''
  fi
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
