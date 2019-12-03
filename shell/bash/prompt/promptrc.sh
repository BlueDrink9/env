# vim: set ft=sh:
# vim:ts=2:sw=2
#
# Don't load normal prompt if liquidprompt installed.

# # If not running interactively, don't do anything
case $- in
  *i*) ;;
  *) return;;
esac

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

source "${SCRIPT_DIR}"/../../variables.sh

# Used when waiting for user input with "select"
export PS3="Select: "
# Used with -x for debugging bash
export PS4='+(${BASH_SOURCE}:${LINENO}): ${FUNCNAME[0]:+${FUNCNAME[0]}(): }'

# \e]0 escapes to window title, \a ends it.
export WINDOW_TITLE_BASH_PATH="\[\e]2;[\W] \u@\h: [\w] ${GIT_BRANCH} – Bash\a\]"

# Truncate paths with '...', leaving only the last n folders in prompt
# Not actually desirable, since I only store the full path in the window bar
# PROMPT_DIRTRIM=5

# Used for set -o, shows a symbol at start of prompt for bash vi mode
# RLVersion=$(readline --version)
if  compareVersionNum $BASH_VERSION_CLEAN '>' 4.2 ; then
  # Introduced in readline 6.3, bash 4.3
  bind 'set show-mode-in-prompt on'
  if  compareVersionNum $BASH_VERSION_CLEAN '>' 4.3 ; then
    # Introduced in readline 7, bash 4.4
    if [ ! -z "$term_bar_cursor" ]; then
      bind "set vi-ins-mode-string \"\1${term_bar_cursor}\2\""
      bind "set vi-cmd-mode-string \"\1${term_block_cursor}\2\""
    else
      # Same as airline colours (solarized).
      bind "set vi-ins-mode-string \"\1${White}${bg_Yellow}\2++\1${NC}\2\""
      bind "set vi-cmd-mode-string \"\1${White}${bg_Green}\2::\1${NC}\2\""
    fi
  fi
fi

if [ -z "${LIQUIDPROMPT_DIR}" ]; then
  LIQUIDPROMPT_DIR="$HOME/.config/liquidprompt/"
fi
if [ -s "${LIQUIDPROMPT_DIR}/liquidprompt" ]; then
  source "${LIQUIDPROMPT_DIR}/liquidprompt"
  # Don't load normal prompt if liquidprompt installed.
  return
fi

. "${SCRIPT_DIR}/custom_prompt.sh"
