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
  print -Pn "${escape_start}[%1~] %n@%M: [%~] $JOBS - $shell${escape_end}"
}
precmd_functions+=(_set_window_title)

setopt hist_verify appendhistory
setopt SHARE_HISTORY
# HISTORY_* set in shell settings.sh file.
HISTFILE="${HISTORY_FILE}"
SAVEHIST="${HISTORY_FILESIZE}"
HISTORY_IGNORE="${HISTORY_IGNORE}"
setopt HIST_IGNORE_SPACE HIST_IGNORE_DUPS
setopt no_clobber
setopt autocd nomatch
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
