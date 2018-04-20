# vim: set ft=sh:
# vim:ts=2:sw=2
#
# pblueDrink9 custom bash prompt. Relies on 2 other files.
# Assumes a solarised terminal, with 16 colours.

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

source ${SCRIPT_DIR}/colour_variables.sh
# source ${SCRIPT_DIR}/functions.sh

FLASHING="\[\E[5m\]"

prompt_escape(){
  echo "\\[$1\\]"
}

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

  # escape_colours
  USER_AT_HOST="${pblue}\u${pNC}@${pyellow}\h${pNC}"

  # if git exists (it doesn't on iOS).
  if hash git 2>/dev/null; then
    GIT_BRANCH="`get_git_branch`"
    if [ ! "${GIT_BRANCH}" == "" ]
    then
      # Sets GIT_PROMPT
      parse_git_branch
      GIT_STATUS_PROMPT="${GIT_PROMPT}"
    else
      GIT_STATUS_PROMPT=""
    fi
  fi

  #set_git_prompt
  #GIT_STATUS_PROMPT="${BRANCH}"
  # \e]0 escapes to window title, \a ends it.
  WINDOW_TITLE_BASH_PATH="\[\e]2;[\W] \u@\h: [\w] ${GIT_BRANCH} – Bash\a\]"
  CURR_FULL_PATH="\w"
  CURR_DIR="\W"
  TIME_PROMPT="\t"

  # Set the bash prompt variable.
  # Space left after title is actually for start of prompt.
  # Gives space between vi +: and time.
  PS1="${WINDOW_TITLE_BASH_PATH} ${pwhite}${pbg_Black}${TIME_PROMPT}${pNC} ${USER_AT_HOST}: ${PREV_COMMAND_COLOUR}[${CURR_DIR}]${pNC}${GIT_STATUS_PROMPT} ${PROMPT_SYMBOL}"
}

# Return the prompt symbol ($) to use, colorized based on the return value of the
# previous command.
set_prompt_symbol () {
  if [ $1 -eq 0 ] ; then
    PROMPT_SYMBOL="${pgreen}\\$ ${pNC}";
    PREV_COMMAND_COLOUR="${pgreen}";
  else
    PROMPT_SYMBOL="${pred}\\$ ${NC}";
    PREV_COMMAND_COLOUR="${pred}";
  fi
}

# Tell bash to execute this function just before displaying its prompt.
PROMPT_COMMAND=set_bash_prompt
if hash tmux >&2 /dev/null; then
  PROMPT_COMMAND=$PROMPT_COMMAND && tmux rename-window "$WINDOW_TITLE_BASH_PATH"
fi

# get current branch in git repo
# Check status of branch
# pgreen if no changes, yellow if modified. Cyan if there are misc changes to files.
parse_git_branch() {
  STATUS_COLOUR=${pNC}
  BRANCH=`get_git_branch`
  if [ ! "${BRANCH}" == "" ]
  then
    status=`git status 2>&1 | tee`
    dirty=`echo -n "${status}" 2> /dev/null | grep "modified:" &> /dev/null; echo "$?"`
    clean=`echo -n "${status}" 2> /dev/null | grep "clean" &> /dev/null; echo "$?"`
    untracked=`echo -n "${status}" 2> /dev/null | grep "Untracked files" &> /dev/null; echo "$?"`
    ahead=`echo -n "${status}" 2> /dev/null | grep "Your branch is ahead of" &> /dev/null; echo "$?"`
    behind=`echo -n "${status}" 2> /dev/null | grep "Your branch is behind" &> /dev/null; echo "$?"`
    diverged=`echo -n "${status}" 2> /dev/null | grep "diverged" &> /dev/null; echo "$?"`
    newfile=`echo -n "${status}" 2> /dev/null | grep "new file:" &> /dev/null; echo "$?"`
    renamed=`echo -n "${status}" 2> /dev/null | grep "renamed:" &> /dev/null; echo "$?"`
    deleted=`echo -n "${status}" 2> /dev/null | grep "deleted:" &> /dev/null; echo "$?"`
    bits=''
    if [ "${clean}" == "0" ]; then
      STATUS_COLOUR=${pgreen}
      bits=""
    fi
    if [ "${ahead}" == "0" ]; then
      STATUS_COLOUR=${pcyan}
      bits="^${bits}"
    fi
    if [ "${behind}" == "0" ]; then
      STATUS_COLOUR=${pcyan}
      bits="v${bits}"
    fi
    if [ "${diverged}" == "0" ]; then
      STATUS_COLOUR=${pcyan}
      bits="^v${bits}"
			# optional: use several other possible unicode symbols.
    fi
    if [ "${untracked}" == "0" ]; then
      bits="?${bits}"
    fi
    if [ "${renamed}" == "0" ]; then
      bits=">${bits}"
    fi
    if [ "${deleted}" == "0" ]; then
      bits="X${bits}"
    fi
    if [ "${newfile}" == "0" ]; then
      bits="+${bits}"
    fi
    if [ "${dirty}" == "0" ]; then
      STATUS_COLOUR=${pyellow}
      bits="*${bits}"
    fi
    if [ ! "${bits}" == "" ] || [ "${clean}" == "0" ]; then
      STATUS="${bits}"
    else
      STATUS="!"
    fi

    # Sometimes status can be slow. Consider removing.
    GIT_PROMPT="${STATUS_COLOUR}{${BRANCH}${STATUS}}${pNC}"
  else
    GIT_PROMPT=""
  fi
}


