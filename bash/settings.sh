# -*-  mode: shell-script; -*-
# vim: ft=sh:fdm=marker:fmr={[},{]}

BASH_VERSION_CLEAN="${BASH_VERSION//[^0-9.]*/}"

# test if this is an ssh shell
if [ -n "$SSH_CLIENT" ] || [ -n "$SSH_TTY" ]; then
  export SESSION_TYPE=remote/ssh
  export SSHSESSION=1
  # many other tests omitted
else
  # case $(ps -o comm= -p $PPID) in
  # case $(cat /proc/$PPID/comm) in
  # SESSION_TYPE=remote/ssh;;
  # esac
  if substrInStr "darwin1" "$OSTYPE"; then
    parent="$(ps -o ppid,comm | grep $PPID)"
  else
    parents="$(pstree -p | grep $PPID)"
  fi
  if substrInStr "sshd" "$parents"; then
    SESSION_TYPE=remote/ssh
  fi
fi

# You know it, baby. Shouldn't need to use nano ever
# export EDITOR="vim --noplugin --cmd \"let g:noPlugins=1\""
# export VISUAL="vim --cmd \"let g:liteMode=1\""
export VISUAL="vim"
export GIT_EDITOR="$VISUAL"
export XDG_CONFIG_HOME="$HOME/.config"

# {[} Terminal settings
# Set defaults here for various terms
if substrInStr "Apple" "$TERM_PROGRAM"; then
  COLORTERM=16
fi
if substrInStr "kitty" "$TERM"; then
  COLORTERM="truecolor"
  USENF=${USENF:-1}
fi
# Termux
if substrInStr "Android" "$(uname -a)";  then
  export ISTERMUX=1
  export CLIP_PROGRAM_COPY="termux-clipboard-set"
  export CLIP_PROGRAM_PASTE="termux-clipboard-get"
  export HOSTNAME="$(getprop net.hostname)"
  export HOST="${HOSTNAME}"
  if [ -z "$SSHSESSION" ]; then
    export NOTMUX=1
    COLORTERM="truecolor"
  fi
fi
# export COLORTERM USENF USEPF

USENF=${USENF:-0}
USEPF=${USEPF:-0}
COLORTERM=${COLORTERM:-16}
TERM_PROGRAM=${TERM_PROGRAM:-}
# Will get exported to ssh servers (see functions->export_termoptions)
export TERMOPTIONS=(USENF USEPF COLORTERM TERM_PROGRAM)

if [ -n "$TMUX" ]; then
  # In a tmux session
  set_tmux_termoptions
fi

# {]} Terminal settings

# Source .dir_colours
if [ -x /usr/bin/dircolors ]; then
  test -r ~/.dircolours_solarized && eval "$(dircolors -b ~/.dircolours_solarized)" || eval "$(dircolors -b)"
fi
source "$DOTFILES_DIR/terminal/x/linuxterm.sh"

export LC_CTYPE="en_US.UTF-8"
# Allow sending ctrl+S to applications in terminal (prev stops scrolling).
# Only when interactive.
if [[ $- == *i* ]]; then stty -ixon; fi

if [[ -z $DISPLAY ]] && [[ $(tty) = /dev/tty1 ]]; then
  export XKB_DEFAULT_LAYOUT="us,us"
  export XKB_DEFAULT_VARIANT="colemak,"
  export XKB_DEFAULT_OPTIONS="grp:alt_shift_toggle,caps:backspace,grp_led:caps,altwin:swap_alt_win"
fi

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
export HISTSIZE=1000
export HISTFILESIZE=2000
# don't put duplicate lines or lines starting with space in the history.
# See bash(1) for more options
export HISTCONTROL=ignoredups:erasedups
# Don't record some commands
export HISTIGNORE="&:[ ]*:exit:ls:bg:fg:history:clear"
if [ ! -d "${HOME}/.logs" ] ; then
  mkdir ${HOME}/.logs
fi
PROMPT_COMMAND="log_command"
# Save to history when command is executed, rather than when terminal is closed.
# Then reread it.
# PROMPT_COMMAND="${PROMPT_COMMAND} && history -a; history -c; history -r"
PROMPT_COMMAND="${PROMPT_COMMAND} && _bash_history_sync"
# append to the history file, don't overwrite it
shopt -s histappend
# Save multi-line commands as one command
shopt -s cmdhist

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize
# # If set, the pattern "**" used in a pathname expansion context will
# # match all files and zero or more directories and subdirectories.
#shopt -s globstar

# No init prevents screen being cleared on enter/exit.
# Window=4 is scrolling buffer.
# export LESS='--quit-if-one-screen --ignore-case --status-column --HILITE-UNREAD --tabs=4 --window=-4'
export LESS='--quit-if-one-screen --ignore-case --status-column --LONG-PROMPT --HILITE-UNREAD --tabs=4 --no-init --window=-4 --RAW-CONTROL-CHARS'
# make less more friendly for non-text input files (eg .gz). See lesspipe(1)
# Different lesspipe scripts, like GNU source-highlight, give syntax highlighting
# if type code2color >/dev/null 2>&1; then
# This variable only works with https://github.com/wofr06/lesspipe
# c2c is the default. Change to set to something else, eg pygmentizer (slow)
# export LESSCOLORIZER='code2color'
# fi
# Debian's way of doing it...
[ -x lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"
# Brew's way of doing it...
[ -x lesspipe.sh ] && export LESSOPEN="|lesspipe.sh %s" LESS_ADVANCED_PREPROCESSOR=1

if [[ "$OSTYPE" =~ "darwin1" ]]; then  # OSX specific stuff
  # Solarized ls dircolours (sort of)
  export CLICOLOR=1
  # Recommended... but wrong?
  # export LSCOLORS=gxfxbEaEBxxEhEhBaDaCaD
  # Custom, created from comparing website and ls man
  export LSCOLORS=exgxbAbAcxbhxbhBhDhcea
elif [ "$OSTYPE" = "linux-gnu" ]; then  # Linux specific stuff
  true
fi

# enable programmable smart completion features
if ! shopt -oq posix; then
  if [ -f $HOMEBREW_PREFIX/share/bash_completion ]; then
    . $HOMEBREW_PREFIX/share/bash_completion # version 2
  elif [ -f $HOMEBREW_PREFIX/etc/bash_completion ]; then
    . $HOMEBREW_PREFIX/etc/bash_completion # version 1
  elif [ -f /usr/share/bash-completion/bash_completion ]; then
    . /usr/share/bash-completion/bash_completion
  elif [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
  fi
fi
if [[ $- == *i* ]]; then
  if compareVersionNum ${BASH_VERSION_CLEAN} '<' 4.3; then
    # Remove tab menu completion cycling.
    # Will just complete to common subsequence instead.
    bind 'Tab: complete'
  fi
fi

# TODO Maybe bad... should you mess with $TERM?
# [[ -n "$DISPLAY" && "$TERM" = "xterm" ]] && export TERM=xterm-256color

# make less more friendly for non-text input files, see lesspipe(0)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

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

if [ -n "$DISPLAY" ]; then
  if [ "$OSTYPE" = "linux-gnu" ] && [ $(command -v xorg 2>/dev/null) ]; then
    setxkbmap -layout 'us, us' -variant 'colemak,' -option caps:backspace -option altwin:swap_alt_win -option grp:alt_shift_toggle -option grp_led:caps
  fi
elif [ $(command -v loadkeys 2>/dev/null) ] && [ "$TERM" = linux ]; then
  loadkeys colemak
fi
