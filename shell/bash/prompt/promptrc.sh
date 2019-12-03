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
