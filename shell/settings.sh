#!/usr/bin/env sh
# vim: ft=sh:fdm=marker:fmr={[},{]}

# Prevent duplicating
PROMPT_COMMAND=""

export XDG_CONFIG_HOME="$HOME/.config"

# Will get exported to ssh servers (see functions->export_termoptions)
export TERMOPTIONS=(USENF USEPF COLORTERM TERM_PROGRAM COLOURSCHEME)
# {[} Terminal settings

USENF=${USENF:-}
USEPF=${USEPF:-}
COLORTERM=${COLORTERM:-16}
TERM_PROGRAM=${TERM_PROGRAM:-}
export $TERMOPTIONS

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

# Check if base16 scheme has been set, set COLOURSCHEME.
export PROMPT_COMMAND="base16_colourscheme_set; ${PROMPT_COMMAND}"

export LC_CTYPE="en_US.UTF-8"

if [ -n "$DISPLAY" ]; then
  if [ "$OSTYPE" = "linux-gnu" ] && [ $(command -v xorg 2>/dev/null) ]; then
    # Keyboard setup stuff
    export XKB_DEFAULT_LAYOUT="us,us"
    export XKB_DEFAULT_VARIANT="colemak,"
    export XKB_DEFAULT_OPTIONS="grp:alt_shift_toggle,caps:backspace,grp_led:caps,altwin:swap_alt_win"
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

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
export HISTSIZE=1000
export HISTORY_FILESIZE=5000
export HISTORY_IGNORE="&:[ ]*:exit:ls:bg:fg:history:clear"
if [ ! -d "${HOME}/.logs" ] ; then
  mkdir ${HOME}/.logs
fi
HISTORY_FILE=~/.logs/${shell}_history
export PROMPT_COMMAND="log_command; ${PROMPT_COMMAND}"

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

# make less more friendly for non-text input files, see lesspipe(0)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

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
