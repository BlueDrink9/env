# vim: set ft=sh:
# vim:ts=2:sw=2
#
# blueDrink9 custom bash prompt. Relies on 2 other files.
# Assumes a solarised terminal, with 16 colours.

source ${SCRIPT_DIR}/../colour_variables.sh
# For substrInStr
source ${SCRIPT_DIR}/../functions.sh

# Set the full bash prompt.
set_bash_prompt () {
  # Set the PROMPT_SYMBOL variable. We do this first so we don't lose the
  # return value of the last command.
  set_prompt_symbol $?

  # if git exists (it doesn't on iOS).
  if hash git 2>/dev/null; then
    GIT_BRANCH="$(get_git_branch)"
    if [ ! "${GIT_BRANCH}" == "" ]
    then
      GIT_STATUS_PROMPT="$(prompt_parse_git_branch)"
    else
      GIT_STATUS_PROMPT=""
    fi
  fi

  #set_git_prompt
  #GIT_STATUS_PROMPT="${BRANCH}"

  if [ -z ${USER} ] ; then
    export USER=$(id -u -n)
  fi
  USER_AT_HOST="${pblue}\u${pNC}@${pyellow}\h${pNC}"
  USER_COLOURED="${pblue}\u${pNC}"
  USER_INITIAL_COLOURED="${pblue}${USER:0:1}${pNC}"

  if [ -n "$SSHSESSION" ]; then
    export HOST=$(uname -n | cut -d"." -f1)

    if (( ${#HOST}  > 12 )); then
      # Truncate hostname if it is too long.
      HOST=${HOST:0:12}
      HOST_COLOURED="${pNC}@${pyellow}${HOST}${pNC}"
    else
      HOST_COLOURED="${pNC}@${pyellow}\h${pNC}"
    fi
  else
    HOST=""
  fi

  CURR_FULL_PATH="\w"
  CURR_DIR="\W"
  CURR_DIR_COLOURED="${PREV_COMMAND_COLOUR}[${CURR_DIR}]${pNC}"
  TIME_PROMPT="\t"
  TIME_PROMPT_COLOURED="${pwhite}${pbg_Black}${TIME_PROMPT}${pNC}"
  # VI_MODE is empty var, but is here to remind you that it will exist
  # in the actual prompt because of inputrc settings.
  VI_MODE=""

  # declare -a PROMPT_STRING=(\ "${TIME_PROMPT_COLOURED}: "\ "${USER_AT_HOST}: "\ "${PREV_COMMAND_COLOUR}[${CURR_DIR}]${pNC}"\ "${GIT_STATUS_PROMPT} "\ "${PROMPT_SYMBOL}"\)

  # PROMPT_STATICLEN="${VI_MODE} ${TIME_PROMPT_COLOURED}: ${USER_AT_HOST}: ${CURR_DIR_COLOURED}${GIT_STATUS_PROMPT} ${PROMPT_SYMBOL}"


  # Dynamically build prompt based on the screen size and size of prompt.

  DESIRED_COMMAND_SPACE=40
  curr_dir=result=${PWD##*/}
  if [ ! -z "$GIT_STATUS_PROMPT" ]; then
    # Contains roughly 20 escape chars.
    git_len=$((${#GIT_STATUS_PROMPT} - 20))
  else
    git_len=0
  fi
  # + 9 is for the extra few symbols that make up the prompt.
  prompt_len_no_time_host_user=$(( ${#curr_dir} + ${git_len} + 7 ))
  prompt_len_no_time_host=$(($prompt_len_no_time_host_user + 1 + ${#USER}))
  prompt_len_no_time_user=$(($prompt_len_no_time_host_user + 1 + ${#HOST}))
  # prompt_len_no_time=$(( $prompt_len_no_time_host + ${#HOST} + 1 ))
  prompt_len_no_time=$(( $prompt_len_no_time_user + ${#USER} + 1 ))
  # + 8 is number of chars in the time_prompt
  prompt_len=$(( $prompt_len_no_time + 8 ))

  VAR_PROMPT=""

  if (( $((${COLUMNS} - $prompt_len)) > ${DESIRED_COMMAND_SPACE})); then
    VAR_PROMPT="${TIME_PROMPT_COLOURED} ${USER_COLOURED}${HOST_COLOURED}:"
  elif (( $((${COLUMNS} - $prompt_len_no_time)) > ${DESIRED_COMMAND_SPACE})); then
    VAR_PROMPT="${USER_COLOURED}${HOST_COLOURED}:"
  elif (( $((${COLUMNS} - $prompt_len_no_time_host)) > ${DESIRED_COMMAND_SPACE})); then
    VAR_PROMPT="${USER_INITIAL_COLOURED}${HOST_COLOURED}:"
    # elif (( $((${COLUMNS} - $prompt_len_no_time_host)) > ${DESIRED_COMMAND_SPACE})); then
    #   VAR_PROMPT="${USER_COLOURED}: "
    # let "remaining_space= ${COLUMNS} - $prompt_len_no_time_host_user"
  else
    VAR_PROMPT=""
  fi

  # number of bg jobs, or "" if 0.
  JOBS=$(if [ -n "$(jobs -p)" ]; then echo "\j"; fi)

  # PROMPT_STATICLEN="${VI_MODE} ${TIME_PROMPT_COLOURED}: ${USER_AT_HOST}: ${CURR_DIR_COLOURED}${GIT_STATUS_PROMPT} ${PROMPT_SYMBOL}"
  PROMPT="${VI_MODE} ${VAR_PROMPT} ${CURR_DIR_COLOURED}${GIT_STATUS_PROMPT} ${JOBS}${PROMPT_SYMBOL}"

  # Ensures you won't have prompt displaced by previous line's input (eg ^C)
  cursorToBoL="\[\033[G\]"
  # Set the bash prompt variable.
  # PS1="${WINDOW_TITLE_BASH_PATH}${cursorToBoL}${PROMPT}"
  PS1="${WINDOW_TITLE_BASH_PATH}${PROMPT}"
  # PS1="${WINDOW_TITLE_BASH_PATH}${PROMPT_STATICLEN}"
  unset GIT_BRANCH GIT_STATUS_PROMPT USER_AT_HOST \
    USER_COLOURED USER_INITIAL_COLOURED HOST_COLOURED CURR_FULL_PATH CURR_DIR \
    CURR_DIR_COLOURED TIME_PROMPT TIME_PROMPT_COLOURED VI_MODE \
    DESIRED_COMMAND_SPACE git_len CURR_DIR prompt_len_no_time_host_user \
    prompt_len_no_time_host prompt_len_no_time_user prompt_len_no_time \
    prompt_len VAR_PROMPT JOBS PROMPT cursorToBoL 
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

source ${SCRIPT_DIR}/git_prompt.sh

# Tell bash to execute this function just before displaying its prompt.
PROMPT_COMMAND="set_bash_prompt; ${PROMPT_COMMAND}"

# if hash tmux > /dev/null 2>&1 && tmux info > /dev/null 2>&1; then
#   PROMPT_COMMAND=$PROMPT_COMMAND && tmux rename-window "$WINDOW_TITLE_BASH_PATH"
# fi

