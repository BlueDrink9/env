source ${SCRIPT_DIR}/../settings.sh
# Enable using bash's PROMPT_COMMAND.
_prompt_command() { eval "$PROMPT_COMMAND" }
precmd_functions+=(_prompt_command)
# Put info in window title.
_set_window_title(){ echo -ne }
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
