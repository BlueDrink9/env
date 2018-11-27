# -*-  mode: shell-script; -*-
# vim: ft=sh:fdm=marker:fmr={[},{]}

BASH_VERSION_CLEAN="${BASH_VERSION//[^0-9.]*/}"

# You know it, baby. Shouldn't need to use nano ever
export EDITOR="vim --noplugin --cmd \"let g:noPlugins=1\""
export VISUAL="vim --cmd \"let g:liteMode=1\""
export GIT_EDITOR="vim --cmd \"let g:liteMode=1\""

# {[} Terminal settings
if substrInStr "Apple" "$TERM_PROGRAM"; then
    export COLORTERM=16
fi
USENF=${USENF-0}
USEPF=${USEPF-0}
COLORTERM=${COLORTERM-16}
TERM_PROGRAM=${TERM_PROGRAM-}
TERMOPTIONS=(USENF USEPF COLORTERM TERM_PROGRAM)
# {[} Exporting for ssh
EXPORT_TERMOPTIONS=""
for option in ${TERMOPTIONS[*]}; do
    # The E in front of the option is to let it be set without overriding.
    # EXPORT_TERMOPTIONS="${EXPORT_TERMOPTIONS} env E${option}=${!option} "
    EXPORT_TERMOPTIONS="${EXPORT_TERMOPTIONS} export ${option}=${!option}; "
    # This part is used for ssh, and sets the option from the exported var.
    if [[ "$SESSION_TYPE" = "remote/ssh" ]]; then
        eopt=E${option}
        # Maybe this should go after attaching tmux? Or before???
        if [ ! -z "$TMUX" ]; then
            tmux setenv ${option} ${!eopt}
        fi
        # export ${option}=${!eopt}
    fi
done
sshn(){
    host=$1
    # Calls default shell, stripping leading '-'
    \ssh -t $host "${EXPORT_TERMOPTIONS} " '${0#-} -l -s'
    # \ssh -t $host "${EXPORT_TERMOPTIONS} " 'echo ${USENF}'
    # \ssh -t $host 'export USENF=hello && bash -l -s'
    # \ssh -t $host 'export USENF=hello && echo $HOST'
}
alias ssh="sshn"
# {]} Exporting for ssh
# {]} Terminal settings

# Source .dir_colours
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolours_solarized && eval "$(dircolors -b ~/.dircolours_solarized)" || eval "$(dircolors -b)"
fi

export LC_CTYPE="en_US.UTF-8"
# Allow sending ctrl+S to applications in terminal (prev stops scrolling)
stty -ixon

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
PROMPT_COMMAND="${PROMPT_COMMAND} && log_command"
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
# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

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

# brew paths
export PATH="$HOMEBREW_PREFIX/bin:$HOMEBREW_PREFIX/sbin:$PATH"
export XDG_DATA_DIRS="/$HOMEBREW_PREFIX/share:$XDG_DATA_DIRS"
export MANPATH="$HOMEBREW_PREFIX/share/man:$MANPATH"
export INFOPATH="$HOMEBREW_PREFIX/share/info:$INFOPATH"

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

if compareVersionNum ${BASH_VERSION_CLEAN} '<' 4.3; then
    # Remove tab menu completion cycling.
    # Will just complete to common subsequence instead.
    bind 'Tab: complete'
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
