# Underscores are ignored in zsh options
source ${SCRIPT_DIR}/../settings.sh
# Enable using bash's PROMPT_COMMAND.
_prompt_command() { eval "$PROMPT_COMMAND" }
precmd_functions+=(_prompt_command)
DISABLE_AUTO_TITLE="true"
# Put info in window title. Curr dir for taskbar quicklook, then full info.
_set_window_title(){
  if [ -n "$TMUX" ]; then
    local escape_start='\e]2;'
    local escape_end='\a'
  else
    local escape_start='\033]'
    local escape_end='\007'
  fi
  # number of bg jobs, or "" if 0.
  JOBS=$(if [ -n "$(jobs -p)" ]; then echo "%j"; fi)
  print -Pn "${escape_start}${WINDOW_CUSTOM_NAME}[%1~] %n@%M: [%~] $JOBS - $SHELL_PROGRAM${escape_end}"
}
precmd_functions+=(_set_window_title)

setopt hist_verify appendhistory
# HISTORY_* set in shell settings.sh file.
HISTFILE="${HISTORY_FILE}"
SAVEHIST="${HISTORY_FILESIZE}"
# Zsh HISTORY_IGNORE uses regex (a|b) for separate patterns.
HISTORY_IGNORE="($(echo ${HISTORY_IGNORE_PATTERNS} | tr ':' '|'))"
setopt HIST_IGNORE_SPACE HIST_IGNORE_DUPS
setopt hist_no_store histreduceblanks
# Append to history after every command, not just when shell exits.
# Set by SHARE_HISTORY
# setopt inc_append_history
setopt SHARE_HISTORY
setopt no_clobber
setopt autocd nomatch
setopt cdable_vars
unsetopt beep notify
# Disable ctrl+s for stopping flow.
setopt noflowcontrol
# Allows ? to be used in commands. Otherwise is a glob meaning 'any char'.
# But escaping the ? works in both bash and zsh.
# unsetopt nomatch
# Don't pushd dirs already on the pushd stack.
setopt PUSHD_IGNORE_DUPS
setopt AUTO_PARAM_SLASH
setopt AUTO_LIST
unsetopt LIST_AMBIGUOUS
setopt CHECK_JOBS
# Default includes most punctuation and symbols, dashes, slashes etc. I don't want this.
# Affects what chars are considered part of a 'word' for ctrl w etc.
WORDCHARS='_$'
setopt ALWAYS_TO_END 
# Allows # comments during cmd line
setopt interactivecomments
