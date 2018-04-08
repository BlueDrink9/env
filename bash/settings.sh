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
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolours_solarized && eval "$(dircolors -b ~/.dircolours_solarized)" || eval "$(dircolors -b)"
fi

export LC_CTYPE="en_US.UTF-8"
# Allow sending ctrl+S to applications in terminal (prev stops scrolling)
stty -ixon
# don't put duplicate lines or lines starting with space in the history.
# See bash(1) for more options

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=1000
HISTFILESIZE=2000
export HISTCONTROL=ignoredups:erasedups
if [ ! -d "${HOME}/.logs" ] ; then
   mkdir ${HOME}/.logs
fi
PROMPT_COMMAND="${PROMPT_COMMAND} && log_command"
# Save to history when command is executed, rather than when terminal is closed.
# Then reread it.
# PROMPT_COMMAND="${PROMPT_COMMAND} && history -a; history -c; history -r"
PROMPT_COMMAND="${PROMPT_COMMAND} && _bash_history_sync"
# append to the history file, don't overwrite it
shopt -s histappend

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize
# If set, the pattern "**" used in a pathname expansion context will
# match all files and zero or more directories and subdirectories.
#shopt -s globstar
# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"
export GREP_OPTIONS='--color=auto'

if [[ "$OSTYPE" =~ "darwin1" ]]; then  # OSX specific stuff
    # Solarized ls dircolours (sort of)
    export CLICOLOR=1
    # Recommended... but wrong?
    # export LSCOLORS=gxfxbEaEBxxEhEhBaDaCaD
    # Custom, created from comparing website and ls man
    export LSCOLORS=exgxbAbAcxbhxbhBhDhcea
    export HOMEBREW_PREFIX="$HOME/homebrew"
    # Make esc act as backspace in terminal
    # todo
    # Remove tab menu completion :( (Doesn't work on OSX).
    bind 'Tab: complete'

elif [ "$OSTYPE" = "linux-gnu" ]; then  # Linux specific stuff
   # Linuxbrew paths
   export HOMEBREW_PREFIX="$HOME/.linuxbrew"
fi

# brew paths
export PATH="$HOMEBREW_PREFIX/bin:$HOMEBREW_PREFIX/sbin:$PATH"
export XDG_DATA_DIRS="/$HOMEBREW_PREFIX/share:$XDG_DATA_DIRS"
export MANPATH="$HOMEBREW_PREFIX/share/man:$MANPATH"
export INFOPATH="$HOMEBREW_PREFIX/share/info:$INFOPATH"

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
  elif [ -f $HOMEBREW_PREFIX/etc/bash_completion ]; then
      . $HOMEBREW_PREFIX/etc/bash_completion
  fi
fi
# TODO Maybe bad... should you mess with $TERM?
[[ -n "$DISPLAY" && "$TERM" = "xterm" ]] && export TERM=xterm-256color

# make less more friendly for non-text input files, see lesspipe(0)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"
