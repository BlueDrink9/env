# -*-  mode: shell-script; -*-
# vim: set ft=sh:

# Enable smart completion in bash
if [ -f /etc/bash_completion ]; then
 . /etc/bash_completion
fi

# You know it, baby. Shouldn't need to use nano ever
export EDITOR=vim
export VISUAL=vim
export GIT_EDITOR=vim
git config --global core.editor "vim"

# Source .dir_colours
eval `dircolors ~/.dircolours_solarized`

# Keyboard setup stuff
capsToBS="-option caps:backspace"
altWinSwap="-option altwin:swap_alt_win"
altShiftToggle="-option grp:alt_shift_toggle"
capsLed="-option grp_led:caps"
colemak="-layout 'us, us' -variant 'colemak,'"
alias wwkb="setxkbmap $colemak $capsToBS $altWinSwap $altShiftToggle $capsLed"

alias term="xfce4-terminal"
export LC_CTYPE="en_US.UTF-8"

# Allow sending ctrl+S to applications in terminal (prev stops scrolling)
stty -ixon

# don't put duplicate lines or lines starting with space in the history.
# See bash(1) for more options
HISTCONTROL=ignoreboth

if [ ! -d "${HOME}/.logs" ] ; then
   mkdir ${HOME}/.logs
fi
PROMPT_COMMAND="${PROMPT_COMMAND} && log_command"

# Save to history when command is executed, rather than when terminal is closed.
PROMPT_COMMAND="history -a && ${PROMPT_COMMAND}"

# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=1000
HISTFILESIZE=2000

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# If set, the pattern "**" used in a pathname expansion context will
# match all files and zero or more directories and subdirectories.
#shopt -s globstar

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# Add an "alert" alias for long running commands.  Use like so:
#   sleep 10; alert
alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'


# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if ! shopt -oq posix; then
  if [ -f /usr/share/bash-completion/bash_completion ]; then
    . /usr/share/bash-completion/bash_completion
  elif [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
  fi
fi
# Maybe bad... should you mess with $TERM?
[[ -n "$DISPLAY" && "$TERM" = "xterm" ]] && export TERM=xterm-256color
