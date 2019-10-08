# -*-  mode: shell-script; -*-
# vim: ft=sh:fdm=marker:fmr={[},{]}

BASH_VERSION_CLEAN="${BASH_VERSION//[^0-9.]*/}"

# Prevent duplicating
PROMPT_COMMAND=""

export XDG_CONFIG_HOME="$HOME/.config"

# {[} Terminal settings

USENF=${USENF:-}
USEPF=${USEPF:-}
COLORTERM=${COLORTERM:-16}
TERM_PROGRAM=${TERM_PROGRAM:-}
export $TERMOPTIONS

if [ -n "$TMUX" ]; then
  # In a tmux session
  set_tmux_termoptions
  # if [ "$(echo "$TMUX_VERSION < 2.3" | bc)" == 1 ]; then
  if compareVersionNum "$TMUX_VERSION" "<" "2.3"; then
    if [ "$COLORTERM" == "truecolor" ] || [ "$COLORTERM" == "24bit" ]; then
      COLORTERM=16
    fi
  fi
fi

# {]} Terminal settings

# Check if base16 scheme has been set, set COLOURSCHEME.
export PROMPT_COMMAND="base16_colourscheme_set; ${PROMPT_COMMAND}"

# Source .dir_colours
if [ -x /usr/bin/dircolors ]; then
  test -r ~/.dircolours_solarized && eval "$(dircolors -b ~/.dircolours_solarized)" || eval "$(dircolors -b)"
fi
if [ "$TERM" = "linux" ]; then
  source "$DOTFILES_DIR/terminal/x/linuxterm.sh"
fi

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
# export HISTCONTROL=ignoreboth:erasedups
# Don't record some commands
export HISTIGNORE="&:[ ]*:exit:ls:bg:fg:history:clear"
export HISTCONTROL=ignoredups:erasedups
# append to the history file, don't overwrite it
shopt -s histappend
export PROMPT_COMMAND="bash_history_sync; ${PROMPT_COMMAND}"
if [ ! -d "${HOME}/.logs" ] ; then
  mkdir ${HOME}/.logs
fi
export PROMPT_COMMAND="log_command; ${PROMPT_COMMAND}"
# Save multi-line commands as one command
shopt -s cmdhist
# Always send backspace as ^?, not ^H.
stty erase ^?

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
  export XDG_CONFIG_HOME="$HOME/.config"
elif [ "$OSTYPE" = "linux-gnu" ]; then  # Linux specific stuff
  true
fi

# enable programmable smart completion features
if ! shopt -oq posix; then
  sourceIfReadable(){
    local sIRPath="$1"
    [ -r "${sIRPath}" ] && . "${sIRPath}"
    # unset sIRPath
  }
  # On OSX, bash_completion v2 needs to have the backwards compat directory specified.
  # Tries sourcing completion version 2 (for bash > 4).
  # If it isn't installed, try v1, then try looking for system versions (ie for 
  # non-brewed systems.
  if sourceIfReadable "$HOMEBREW_PREFIX/etc/profile.d/bash_completion.sh"; then
    export readonly BASH_COMPLETION_COMPAT_DIR="$HOMEBREW_PREFIX/etc/bash_completion.d"
    # These always need to be sourced if using v2. V1 apparently does them
    # itself. See https://github.com/Homebrew/homebrew-core/issues/36377.
    for f in "$HOMEBREW_PREFIX/etc/bash_completion.d/"*; do
      sourceIfReadable "$f"
    done
  else
    sourceIfReadable "$HOMEBREW_PREFIX/etc/bash_completion" || \
    sourceIfReadable "/usr/share/bash-completion/bash_completion" || \
    sourceIfReadable "/etc/bash_completion"
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
    # Keyboard setup stuff
    capsToBS="-option caps:backspace"
    altWinSwap="-option altwin:swap_alt_win"
    winSpaceToggle="-option grp:win_space_toggle"
    capsLed="-option grp_led:caps"
    colemak="-layout 'us, us' -variant 'colemak,'"
    setxkbmap $colemak $capsToBS $altWinSwap $winSpaceToggle $capsLed
    unset colemak capsToBS altWinSwap winSpaceToggle capsLed
  fi
elif [ $(command -v loadkeys 2>/dev/null) ] && [ "$TERM" = linux ]; then
  loadkeys colemak
fi

#{[} fzf
# This is added to .bashrc by fzf on installation anyway, and it's easier to disable from there.
# if [ -f ~/.fzf.bash ]; then
# source ~/.fzf.bash
export FZF_CTRL_R_OPTS='--sort'
# fi
if command -v fd >/dev/null 2>&1; then
  export FZF_DEFAULT_COMMAND='fd --type f'
  export FZF_ALT_C_COMMAND='fd --type d . --color=never'
fi
#{]}

# Colour all stderr output red.
# This may be problematic in scripts, actually...
# exec 9>&2
# exec 8> >(
#     while IFS='' read -r line || [ -n "$line" ]; do
#        echo -e "\033[31m${line}\033[0m"
#     done
# )
# function undirect(){ exec 2>&9; }
# function redirect(){ exec 2>&8; }
# trap "redirect;" DEBUG
# PROMPT_COMMAND='undirect;'
