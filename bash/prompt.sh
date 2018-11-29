# vim: set ft=sh:
# vim:ts=2:sw=2
#
# blueDrink9 custom bash prompt. Relies on 2 other files.
# Assumes a solarised terminal, with 16 colours.

# # If not running interactively, don't do anything
case $- in
    *i*) ;;
      *) return;;
esac

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

source ${SCRIPT_DIR}/colour_variables.sh
# source ${SCRIPT_DIR}/functions.sh

# Used when waiting for user input with "select"
export PS3="Select: "
# Used with -x for debugging bash
export PS4='+(${BASH_SOURCE}:${LINENO}): ${FUNCNAME[0]:+${FUNCNAME[0]}(): }'

FLASHING="\[\E[5m\]"

prompt_escape(){
  echo "\\[$1\\]"
}
# Truncate paths with '...', leaving only the last n folders in prompt
# Not actually desirable, since I only store the full path in the window bar
# PROMPT_DIRTRIM=5

# Used for set -o, shows a symbol at start of prompt for bash vi mode
# RLVersion=`readline --version`
if  compareVersionNum $BASH_VERSION_CLEAN '>' 4.2 ; then
  # Introduced in readline 6.3, bash 4.3
  bind 'set show-mode-in-prompt on'
  if  compareVersionNum $BASH_VERSION_CLEAN '>' 4.3 ; then
    # Introduced in readline 7, bash 4.4
    if substrInStr "xterm" "$TERM" || substrInStr "tmux" "$TERM" || [ "$TERM_PROGRAM" == "mintty" ]; then
      bar_cursor="\e[6 q"
      block_cursor="\e[1 q"
    fi
    if [ ! -z "$bar_cursor" ]; then
      bind "set vi-ins-mode-string \"\1${bar_cursor}\2\""
      bind "set vi-cmd-mode-string \"\1${block_cursor}\2\""
    else
      # Same as airline colours (solarized).
      bind "set vi-ins-mode-string \"\1${White}${bg_Yellow}\2++\1${NC}\2\""
      bind "set vi-cmd-mode-string \"\1${White}${bg_Green}\2::\1${NC}\2\""
    fi
  fi
fi


# colourVars="
# Blue
# Yellow
# Green
# On_Black
# NC
# White
# Cyan
# Red
# "
# escape_colours(){
#   # Replace colour sequence with its escaped cousin.
#   colarray=($colourVars)
#   for colour in "${colarray[@]}" ; do
#     printf -v $colour `prompt_escape ${!colour}`
#   done
# }

# Set the full bash prompt.
set_bash_prompt () {
  # Set the PROMPT_SYMBOL variable. We do this first so we don't lose the
  # return value of the last command.
  set_prompt_symbol $?

  # if git exists (it doesn't on iOS).
  if hash git 2>/dev/null; then
    local GIT_BRANCH="`get_git_branch`"
    if [ ! "${GIT_BRANCH}" == "" ]
    then
      local GIT_STATUS_PROMPT="`parse_git_branch`"
      unset GIT_PROMPT
    else
      local GIT_STATUS_PROMPT=""
    fi
  fi

  #set_git_prompt
  #GIT_STATUS_PROMPT="${BRANCH}"
  # \e]0 escapes to window title, \a ends it.
  local WINDOW_TITLE_BASH_PATH="\[\e]2;[\W] \u@\h: [\w] ${GIT_BRANCH} – Bash\a\]"

  if [ -z ${USER} ] ; then
    local USER=`id -u -n`
  fi
  local USER_AT_HOST="${pblue}\u${pNC}@${pyellow}\h${pNC}"
  local USER_COLOURED="${pblue}\u${pNC}"
  local USER_INITIAL_COLOURED="${pblue}${USER:0:1}${pNC}"

  local HOST=`uname -n | cut -d"." -f1`

  if (( ${#HOST}  > 12 )); then
    # Truncate hostname if it is too long.
    local HOST=${HOST:0:12}
    local HOST_COLOURED="${pyellow}${HOST}${pNC}"
  else
    local HOST_COLOURED="${pyellow}\h${pNC}"
  fi

  local CURR_FULL_PATH="\w"
  local CURR_DIR="\W"
  local CURR_DIR_COLOURED="${PREV_COMMAND_COLOUR}[${CURR_DIR}]${pNC}"
  local TIME_PROMPT="\t"
  local TIME_PROMPT_COLOURED="${pwhite}${pbg_Black}${TIME_PROMPT}${pNC}"
  # VI_MODE is empty var, but is here to remind you that it will exist
  # in the actual prompt because of inputrc settings.
  local VI_MODE=""

  # declare -a PROMPT_STRING=(\ "${TIME_PROMPT_COLOURED}: "\ "${USER_AT_HOST}: "\ "${PREV_COMMAND_COLOUR}[${CURR_DIR}]${pNC}"\ "${GIT_STATUS_PROMPT} "\ "${PROMPT_SYMBOL}"\)

  # PROMPT_STATICLEN="${VI_MODE} ${TIME_PROMPT_COLOURED}: ${USER_AT_HOST}: ${CURR_DIR_COLOURED}${GIT_STATUS_PROMPT} ${PROMPT_SYMBOL}"


  # Dynamically build prompt based on the screen size and size of prompt.

  local DESIRED_COMMAND_SPACE=40
  local curr_dir=result=${PWD##*/}
  if [ ! -z "$GIT_STATUS_PROMPT" ]; then
    # Contains roughly 20 escape chars.
    local git_len=$((${#GIT_STATUS_PROMPT} - 20))
  else
    local git_len=0
  fi
  # + 9 is for the extra few symbols that make up the prompt.
  local prompt_len_no_time_host_user=$(( ${#curr_dir} + ${git_len} + 7 ))
  local prompt_len_no_time_host=$(($prompt_len_no_time_host_user + 1 + ${#USER}))
  local prompt_len_no_time_user=$(($prompt_len_no_time_host_user + 1 + ${#HOST}))
  # prompt_len_no_time=$(( $prompt_len_no_time_host + ${#HOST} + 1 ))
  local prompt_len_no_time=$(( $prompt_len_no_time_user + ${#USER} + 1 ))
  # + 8 is number of chars in the time_prompt
  local prompt_len=$(( $prompt_len_no_time + 8 ))

  local VAR_PROMPT=""

  if (( $((${COLUMNS} - $prompt_len)) > ${DESIRED_COMMAND_SPACE})); then
    local VAR_PROMPT="${TIME_PROMPT_COLOURED} ${USER_COLOURED}@${HOST_COLOURED}:"
  elif (( $((${COLUMNS} - $prompt_len_no_time)) > ${DESIRED_COMMAND_SPACE})); then
    local VAR_PROMPT="${USER_COLOURED}@${HOST_COLOURED}:"
  elif (( $((${COLUMNS} - $prompt_len_no_time_host)) > ${DESIRED_COMMAND_SPACE})); then
    local VAR_PROMPT="${USER_INITIAL_COLOURED}@${HOST_COLOURED}:"
  # elif (( $((${COLUMNS} - $prompt_len_no_time_host)) > ${DESIRED_COMMAND_SPACE})); then
  #   VAR_PROMPT="${USER_COLOURED}: "
    # let "remaining_space= ${COLUMNS} - $prompt_len_no_time_host_user"
  else
    local VAR_PROMPT=""
  fi

  # number of bg jobs, or "" if 0.
  local JOBS=`if [ -n "$(jobs -p)" ]; then echo "\j"; fi`

  # PROMPT_STATICLEN="${VI_MODE} ${TIME_PROMPT_COLOURED}: ${USER_AT_HOST}: ${CURR_DIR_COLOURED}${GIT_STATUS_PROMPT} ${PROMPT_SYMBOL}"
  local PROMPT="${VI_MODE} ${VAR_PROMPT} ${CURR_DIR_COLOURED}${GIT_STATUS_PROMPT} ${JOBS}${PROMPT_SYMBOL}"

  # Ensures you won't have prompt displaced by previous line's input (eg ^C)
  local cursorToBoL="\[\033[G\]"
  # Set the bash prompt variable.
  # PS1="${WINDOW_TITLE_BASH_PATH}${cursorToBoL}${PROMPT}"
  PS1="${WINDOW_TITLE_BASH_PATH}${PROMPT}"
  # PS1="${WINDOW_TITLE_BASH_PATH}${PROMPT_STATICLEN}"
}

# Return the prompt symbol ($) to use, colorized based on the return value of the
# previous command.
set_prompt_symbol () {
  if [ $1 -eq 0 ] ; then
    PROMPT_SYMBOL="${pgreen}\\$ ${pNC}";
    PREV_COMMAND_COLOUR="${pgreen}";
  else
    PROMPT_SYMBOL="${pred}\\$ ${pNC}";
    PREV_COMMAND_COLOUR="${pred}";
  fi
}

# Tell bash to execute this function just before displaying its prompt.
PROMPT_COMMAND=set_bash_prompt

# if hash tmux > /dev/null 2>&1 && tmux info > /dev/null 2>&1; then
#   PROMPT_COMMAND=$PROMPT_COMMAND && tmux rename-window "$WINDOW_TITLE_BASH_PATH"
# fi

# get current branch in git repo
# Check status of branch
# pgreen if no changes, yellow if modified. Cyan if there are misc changes to files.
parse_git_branch() {
  local STATUS_COLOUR=${pNC}
  local BRANCH=`get_git_branch`
  if [ ! "${BRANCH}" == "" ]
  then
    local status=`git status 2>&1 | tee`
    local dirty=`echo -n "${status}" 2> /dev/null | grep "modified:" &> /dev/null; echo "$?"`
    local clean=`echo -n "${status}" 2> /dev/null | grep "clean" &> /dev/null; echo "$?"`
    local untracked=`echo -n "${status}" 2> /dev/null | grep "Untracked files" &> /dev/null; echo "$?"`
    local ahead=`echo -n "${status}" 2> /dev/null | grep "Your branch is ahead of" &> /dev/null; echo "$?"`
    local behind=`echo -n "${status}" 2> /dev/null | grep "Your branch is behind" &> /dev/null; echo "$?"`
    local diverged=`echo -n "${status}" 2> /dev/null | grep "diverged" &> /dev/null; echo "$?"`
    local newfile=`echo -n "${status}" 2> /dev/null | grep "new file:" &> /dev/null; echo "$?"`
    local renamed=`echo -n "${status}" 2> /dev/null | grep "renamed:" &> /dev/null; echo "$?"`
    local deleted=`echo -n "${status}" 2> /dev/null | grep "deleted:" &> /dev/null; echo "$?"`
    local bits=''
    if [ "${clean}" == "0" ]; then
      local STATUS_COLOUR=${pgreen}
      local bits=""
    fi
    if [ "${ahead}" == "0" ]; then
      local STATUS_COLOUR=${pcyan}
      local bits="^${bits}"
    fi
    if [ "${behind}" == "0" ]; then
      local STATUS_COLOUR=${pcyan}
      local bits="v${bits}"
    fi
    if [ "${diverged}" == "0" ]; then
      local STATUS_COLOUR=${pcyan}
      local bits="^v${bits}"
      # optional: use several other possible unicode symbols.
    fi
    if [ "${untracked}" == "0" ]; then
      local bits="?${bits}"
    fi
    if [ "${renamed}" == "0" ]; then
      local bits=">${bits}"
    fi
    if [ "${deleted}" == "0" ]; then
      local bits="X${bits}"
    fi
    if [ "${newfile}" == "0" ]; then
      local bits="+${bits}"
    fi
    if [ "${dirty}" == "0" ]; then
      local STATUS_COLOUR=${pyellow}
      local bits="*${bits}"
    fi
    if [ ! "${bits}" == "" ] || [ "${clean}" == "0" ]; then
      local STATUS="${bits}"
    else
      local STATUS="!"
    fi

    # Sometimes status can be slow. Consider removing.
    local GIT_PROMPT="${STATUS_COLOUR}{${BRANCH}${STATUS}}${pNC}"
  else
    local GIT_PROMPT=""
  fi
  echo "${GIT_PROMPT}"
}
