#!/usr/bin/env sh
# vim: ft=sh:fdm=marker:fmr={[},{]}

# Prevent duplicating
PROMPT_COMMAND=""

# Will get exported to ssh servers (see functions->export_termoptions)
export TERMOPTIONS=(USENF USEPF COLORTERM TERM_PROGRAM COLOURSCHEME)
# {[} Terminal settings

USENF=${USENF:-0}
if [ "$USENF" = 1 ]; then
  USEPF=1
else
  USEPF=${USEPF:-0}
fi
COLORTERM=${COLORTERM:-16}
# Tmux overrides TERM_PROGRAM, so in tmux we may have pushed it
# into TERMINAL_PROGRAM
TERM_PROGRAM=${TERMINAL_PROGRAM:${TERM_PROGRAM:-}}
# Export each term option.
export ${TERMOPTIONS?}
# Needed for TERMOPTIONS as a string to export properly.
TERMOPTIONS="$TERMOPTIONS"; export TERMOPTIONS

if [ -n "$TMUX" ]; then
  # In a tmux session
  set_tmux_termoptions
  # if [ "$(echo "$TMUX_VERSION < 2.3" | bc)" = 1 ]; then
  if compareVersionNum "$TMUX_VERSION" "<" "2.3"; then
    if [ "$COLORTERM" = "truecolor" ] || [ "$COLORTERM" = "24bit" ]; then
      COLORTERM=16
    fi
  fi
fi

# {]} Terminal settings

# Used when waiting for user input with "select"
export PS3="Select: "

# Check if base16 or kitty scheme has been set, update term and set
# COLOURSCHEME.
export PROMPT_COMMAND="theme; ${PROMPT_COMMAND}"

export LC_CTYPE="en_US.UTF-8"

if [ -n "$DISPLAY" ]; then
  if [ "$OSTYPE" = "linux-gnu" ] && command -v xorg >/dev/null 2>&1; then
    # Keyboard setup stuff
    export XKB_DEFAULT_LAYOUT="us,us"
    export XKB_DEFAULT_VARIANT="colemak,"
    export XKB_DEFAULT_OPTIONS="grp:win_space_toggle,caps:backspace,grp_led:caps,altwin:swap_alt_win"
    capsToBS="-option caps:backspace"
    altWinSwap="-option altwin:swap_alt_win"
    winSpaceToggle="-option grp:win_space_toggle"
    capsLed="-option grp_led:caps"
    colemak="-layout 'us, us' -variant 'colemak,'"
    allK="setxkbmap $colemak $capsToBS $altWinSwap $winSpaceToggle $capsLed"
    alias setKeyboard="$allK"
    eval "$allK"
    unset allK colemak capsToBS altWinSwap winSpaceToggle capsLed
  fi

elif command -v loadkeys >/dev/null 2>&1 && [ "$TERM" = linux ]; then
  loadkeys colemak
fi

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
export HISTSIZE=1000
export HISTORY_FILESIZE=5000
export HISTORY_IGNORE_PATTERNS="&:[ ]*:exit:ls:bg:fg:history:clear:reset:fc *"
if [ ! -d "${HOME}/.logs" ] ; then
  mkdir ${HOME}/.logs
fi
HISTORY_FILE=~/.logs/${SHELL_PROGRAM}_history
export PROMPT_COMMAND="log_command; ${PROMPT_COMMAND}"

# No init prevents screen being cleared on enter/exit.
# Window=4 is scrolling buffer.
# export LESS='--quit-if-one-screen --ignore-case --status-column --HILITE-UNREAD --tabs=4 --window=-4'
export LESS='--quit-if-one-screen --ignore-case --status-column --LONG-PROMPT --HILITE-UNREAD --tabs=4 --no-init --window=-4 --RAW-CONTROL-CHARS -+S'
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

# Use bat as man pager for syntax highlighting
if command -v bat > /dev/null; then
  # bat->batcat for debian-based systems
  export MANPAGER="sh -c 'col -bx | bat -l man -p'"
  export MANROFFOPT="-c"
fi
export BAT_THEME="OneHalfLight"

if substrInStr "darwin1" "$OSTYPE"; then  # OSX specific stuff
  # Solarized ls dircolours (sort of)
  export CLICOLOR=1
  # Recommended... but wrong?
  # export LSCOLORS=gxfxbEaEBxxEhEhBaDaCaD
  # Custom, created from comparing website and ls man
  export LSCOLORS=exgxbAbAcxbhxbhBhDhcea
elif [ "$OSTYPE" = "linux-gnu" ]; then  # Linux specific stuff
  true
fi
# if command -v vivid > /dev/null; then
#   args=""
#   if [ "$COLORTERM" != "truecolor" ]; then
#     args=" --color-mode" "8-bit"
#   fi
#   if substrInStr ayu "$COLOURSCHEME"; then
#     theme="ayu"
#   else
#     theme="$(echo "$COLOURSCHEME" | tr '_' '-')"
#   fi
#   export LS_COLORS="$(vivid $args generate "$theme")"
#   unset args theme
# fi

export LESSHISTFILE="$XDG_CACHE_HOME"/less/history

# make less more friendly for non-text input files, see lesspipe(0)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

#{[} fzf
if command -v fzf > /dev/null; then
  # Set up fzf key bindings and fuzzy completion
  case "$SHELL" in
    *zsh)
      . <(fzf --zsh) || echo "Error setting up fzf in settings"
      # Extra completions - commadn not found?
      # _fzf_setup_completion path ag git kubectl
      # _fzf_setup_completion dir tree
      ;;
    *bash)
      eval "$(fzf --bash)" || echo "Error setting up fzf in settings"
      ;;
    *)
      ;;
  esac
  export FZF_CTRL_R_OPTS='--sort'
  # Default is **, a bit unergonomic. Still need a tab to start it anyway.
  export FZF_COMPLETION_TRIGGER='*'
  if command -v fd >/dev/null 2>&1; then
    _fzf_compgen_path() {
      fd --hidden --follow --exclude ".git" . "$1"
    }
    _fzf_compgen_dir() {
      fd --type d --hidden --follow --exclude ".git" . "$1"
    }
    # export FZF_DEFAULT_COMMAND='fd --type f --color=never'
    # export FZF_ALT_C_COMMAND='fd --type d . --color=never'
    # export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
  fi
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

export MYFLY_KEY_SCHEME=vim
export MCFLY_FUZZY=true
export MCFLY_INTERFACE_VIEW=BOTTOM
export MCFLY_HISTORY_LIMIT=10000

# Cache pip wheels when built. Especially useful for tox, which installs pip
# packages a lot.
export STANDARD_CACHE_DIR="${XDG_CACHE_HOME:-${HOME}/.cache}/pip"
export WHEELHOUSE="${STANDARD_CACHE_DIR}/wheelhouse"
export PIP_FIND_LINKS="file://${WHEELHOUSE}"
export PIP_WHEEL_DIR="${WHEELHOUSE}"
mkdir -p "$WHEELHOUSE"

if command -v direnv >/dev/null 2>&1; then
  eval "$(direnv hook "$SHELL")" || echo "Error setting up direnv in settings"
fi

