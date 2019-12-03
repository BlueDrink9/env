source ${SCRIPT_DIR}/../settings.sh
setopt hist_verify
setopt SHARE_HISTORY
setopt no_clobber
HISTFILE=~/.zsh_history
setopt appendhistory autocd nomatch
unsetopt beep notify
# Disable ctrl+s for stopping flow.
setopt noflowcontrol
# Allows ? to be used in commands. Otherwise is a glob meaning 'any char'.
# But escaping the ? works in both bash and zsh.
# unsetopt nomatch
