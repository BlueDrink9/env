# vim:ft=sh:tw=78:ts=2
# vim:foldmethod=marker:foldmarker={[},{]}
# Contains bash and cli bootstrapping/init code designed to be run once per
# shell

# Unnecessary, gets checked in bashrc
# [ -n "${PROFILE_LOADED}" ] && return
PROFILE_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

# Exec wipes out function definitions, so kill include guard for profile.
unset SHELL_FUNCTIONS_LOADED
source "${PROFILE_DIR}/functions.sh"

source "${PROFILE_DIR}/../profile.sh"
