# vim:ts=2:sw=2:ft=sh

#######################################
# LIQUID PROMPT DEFAULT TEMPLATE FILE #
#######################################

# Available features:
# LP_BATT battery
# LP_LOAD load
# LP_JOBS screen sessions/running jobs/suspended jobs
# LP_USER user
# LP_HOST hostname
# LP_PERM a colon ":"
# LP_PWD current working directory
# LP_VENV Python virtual environment
# LP_PROXY HTTP proxy
# LP_VCS the content of the current repository
# LP_ERR last error code
# LP_MARK prompt mark
# LP_TIME current time
# LP_TTYN number of current terminal (useful in title for quick switching)
# LP_RUNTIME runtime of last command
# LP_MARK_PREFIX user-defined prompt mark prefix (helpful if you want 2-line prompts)
# LP_PS1_PREFIX user-defined general-purpose prefix (default set a generic prompt as the window title)
# LP_PS1_POSTFIX user-defined general-purpose postfix
# LP_BRACKET_OPEN open bracket
# LP_BRACKET_CLOSE close bracket

# Remember that most features come with their corresponding colors,
# see the README.

# If root, the VCS info has not been collected unless LP_ENABLE_VCS_ROOT is set.

LP_c_pre="${LP_PS1_PREFIX}${LP_TIME}"
LP_c_userhost="${LP_USER}${LP_HOST}"
LP_c_externals="${LP_VENV}${LP_VCS}"
LP_c_mark="${LP_ERR}${LP_MARK_PREFIX}${LP_MARK}${LP_PS1_POSTFIX}"
# LP_c_mark="${LP_MARK_PREFIX}${LP_MARK}"
LP_c_path="${LP_PERM}${LP_BRACKET_OPEN}${LP_PWD}${LP_BRACKET_CLOSE}"
# LP_c_jobs="${LP_COLOR_JOB_R}"
LP_c_jobs=" ${LP_JOBS}"

LP_PS1="${LP_c_pre}${LP_c_userhost}${LP_c_path}${LP_c_externals}${LP_c_jobs}${LP_c_mark}"

# LP_PS1="${LP_PS1}${LP_BRACKET_CLOSE}${LP_VENV}${LP_PROXY}"
# "invisible" parts
# Get the current prompt on the fly and make it a title
# LP_TITLE="$(_lp_title "$LP_PS1")"
# Insert it in the prompt
# LP_PS1="${LP_TITLE}${LP_PS1}"
