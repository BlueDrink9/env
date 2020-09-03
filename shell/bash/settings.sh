# -*-  mode: shell-script; -*-
# vim: ft=sh:fdm=marker:fmr={[},{]}

source "${SCRIPT_DIR}/../settings.sh"

BASH_VERSION_CLEAN="${BASH_VERSION//[^0-9.]*/}"


# Always use aliases.
shopt -s expand_aliases

# Source .dir_colours
if [ -x /usr/bin/dircolors ]; then
  test -r ~/.dircolours_solarized && eval "$(dircolors -b ~/.dircolours_solarized)" || eval "$(dircolors -b)"
fi
if [ "$TERM" = "linux" ]; then
  source "$DOTFILES_DIR/terminal/x/linuxterm.sh"
fi

# Used with -x for debugging shells
export PS4='+($(${CURR_SCRIPT_CMD}):${LINENO}): ${FUNCNAME[0]:+${FUNCNAME[0]}(): }'

# \e]0 escapes to window title, \a ends it.
export WINDOW_TITLE_PATH="\[\e]2;\${WINDOW_CUSTOM_NAME}[\W] \u@\h: [\w] ${GIT_BRANCH} – ${SHELL_PROGRAM}\a\]"

# don't put duplicate lines or lines starting with space in the history.
# See bash(1) for more options
# export HISTCONTROL=ignoreboth:erasedups
# HISTORY_* set in shell settings.sh file.
# Don't record some commands
export HISTIGNORE="${HISTORY_IGNORE_PATTERNS}"
export HISTFILESIZE="${HISTORY_FILESIZE}"
export HISTCONTROL=ignoredups:erasedups
export HISTFILE="${HISTORY_FILE}"
# append to the history file, don't overwrite it
shopt -s histappend
export PROMPT_COMMAND="bash_history_sync; ${PROMPT_COMMAND}"
# Save multi-line commands as one command
shopt -s cmdhist

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize
# # If set, the pattern "**" used in a pathname expansion context will
# # match all files and zero or more directories and subdirectories.
#shopt -s globstar

# {[} Bash completion
# enable programmable smart completion features
if ! shopt -oq posix; then
  sourceIfReadable(){
    local sIRPath="$1"
    [ -r "${sIRPath}" ] && . "${sIRPath}"
    # unset sIRPath
  }
  # On OSX, bash_completion v2 needs to have the backwards compat directory specified.
  # Tries sourcing completion version 2 (for bash > 4).
  # If it isn't installed, try v1, then try looking for system versions (ie for 
  # non-brewed systems.
  if sourceIfReadable "$HOMEBREW_PREFIX/etc/profile.d/bash_completion.sh"; then
    export readonly BASH_COMPLETION_COMPAT_DIR="$HOMEBREW_PREFIX/etc/bash_completion.d"
    # These always need to be sourced if using v2. V1 apparently does them
    # itself. See https://github.com/Homebrew/homebrew-core/issues/36377.
    for f in "$HOMEBREW_PREFIX/etc/bash_completion.d/"*; do
      sourceIfReadable "$f"
    done
  else
    sourceIfReadable "$HOMEBREW_PREFIX/etc/bash_completion" || \
    sourceIfReadable "/usr/share/bash-completion/bash_completion" || \
    sourceIfReadable "/etc/bash_completion"
  fi
fi
if [[ $- == *i* ]]; then
  if compareVersionNum ${BASH_VERSION_CLEAN} '<' 4.3; then
    # Remove tab menu completion cycling.
    # Will just complete to common subsequence instead.
    bind 'Tab: complete'
  fi
  if [ "$TERM_PROGRAM" = "mintty" ]; then
    bind '"\C-_":"\C-W"'
  elif [ "$TERM" = "xterm-kitty" ]; then
    true
    bind '"\C-H":"\C-W"'
  fi
fi
# {]} Bash completion

# Prepend cd to directory names automatically
shopt -s autocd 2> /dev/null
# Correct spelling errors during tab-completion
# shopt -s dirspell 2> /dev/null
# Correct spelling errors in arguments supplied to cd
# shopt -s cdspell 2> /dev/null

# This allows you to bookmark your favorite places across the file system
# Define a variable containing a path and you will be able to cd into it regardless of the directory you're in
shopt -s cdable_vars
# Prevent files from being overwritten by redirection.
set -o noclobber

# Allow sending ctrl+S to applications in terminal (prev stops scrolling).
# Only when interactive.
if [[ $- == *i* ]]; then stty -ixon; fi
# Always send backspace as ^?, not ^H.
stty erase ^\?

