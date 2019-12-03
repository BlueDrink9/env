source ${SCRIPT_DIR}/../settings.sh
setopt hist_verify
setopt SHARE_HISTORY
setopt no_clobber
# Allows ? to be used in commands. Otherwise is a glob meaning 'any char'.
# But escaping the ? works in both bash and zsh.
# unsetopt nomatch
