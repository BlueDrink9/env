# vim:ft=sh:tw=78:ts=2
# vim:foldmethod=marker:foldmarker={[},{]}
# Contains bash and cli bootstrapping/init code designed to be run once per
# shell

# Unnecessary, gets checked in bashrc
# [ -n "${PROFILE_LOADED}" ] && return
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
export DOTFILES_DIR=$(cd "${SCRIPT_DIR}/.." && pwd)

# Exec wipes out function definitions, so kill include guard for profile.
unset BASH_FUNCTIONS_LOADED
source "${SCRIPT_DIR}/functions.sh"

if substrInStr "Microsoft" "$([ -f /proc/version ] && cat /proc/version)"; then
  export isWSL=1
fi

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
    parents="$(ps -o ppid,comm | grep $PPID)"
  else
    parents="$(pstree -p | grep $PPID)"
  fi
  if substrInStr "sshd" "$parents"; then
    SESSION_TYPE=remote/ssh
    export SSHSESSION=1
  fi
fi

if [ -z "$HOMEBREW_PREFIX" ]; then
  if [[ "$OSTYPE" =~ "darwin1" ]]; then  # OSX specific stuff
    export HOMEBREW_PREFIX="$HOME/homebrew"
  elif [ "$OSTYPE" = "linux-gnu" ]; then  # Linux specific stuff
    # Linuxbrew paths
    export HOMEBREW_PREFIX="$HOME/.linuxbrew"
    export HOMEBREW_REPOSITORY="$HOMEBREW_PREFIX/Homebrew"
  fi
  if [ ! -d "$HOMEBREW_PREFIX" ]; then
    unset HOMEBREW_PREFIX
  fi
fi

# {[} Terminal-specific settings
# TODO use case
# Set defaults here for various terms
if substrInStr "kitty" "$TERM"; then
  COLORTERM="truecolor"
  USENF=${USENF:-1}
  TERM_PROGRAM="kitty"
elif substrInStr "Android" "$(uname -a)";  then
  # Termux
  export ISTERMUX=1
  export CLIP_PROGRAM_COPY="termux-clipboard-set"
  export CLIP_PROGRAM_PASTE="termux-clipboard-get"
  export HOSTNAME="$(getprop net.hostname)"
  export HOST="${HOSTNAME}"
  TERM_PROGRAM="termux"
elif [ -n "$ITERM_SESSION_ID" ]; then
  # Sets term prog, colourterm itself.
  USENF=${USENF:-1}
  # Default to dark, for dropdown (which is all I use iTerm for).
  COLOURSCHEME=${COLOURSCHEME:-solarized_dark}
elif substrInStr "Apple" "$TERM_PROGRAM"; then
  COLORTERM=16
elif substrInStr "screen" "$TERM"; then
  unset COLORTERM
fi
# {]} Terminal-specific settings

# OSX doesn't have a $DISPLAY variable by default.
if [ -z "$SSHSESSION" ]; then
  if substrInStr "darwin1" "$OSTYPE"; then
    true
  fi
  if [ -n "$ISTERMUX" ]; then
    COLORTERM="truecolor"
    export NOTMUX=1
  fi
fi

# brew paths. Only before load to avoid loading twice.
if [ -n "$HOMEBREW_PREFIX" ] && ! substrInStr "$HOMEBREW_PREFIX" "${PATH%%:*}" ; then
  export HOMEBREW_CELLAR="$HOMEBREW_PREFIX/Cellar"
  export PATH="$HOMEBREW_PREFIX/bin:$HOMEBREW_PREFIX/sbin:$PATH"
  export XDG_DATA_DIRS="$HOMEBREW_PREFIX/share:$XDG_DATA_DIRS"
  export MANPATH="$HOMEBREW_PREFIX/share/man:$MANPATH"
  export INFOPATH="$HOMEBREW_PREFIX/share/info:$INFOPATH"
fi
# Local (usually manually) installed packages. Should have highest priority.
if ! substrInStr "$HOME/.local" "${PATH}" ; then
  export PATH="$HOME/.local/bin:$HOME/.local/sbin:$PATH"
  export XDG_DATA_DIRS="$$HOME/.local/share:$XDG_DATA_DIRS"
  export MANPATH="$$HOME/.local/share/man:$MANPATH"
  export INFOPATH="$$HOME/.local/share/info:$INFOPATH"
fi

# Check if interactive is part of shell options (running interactively)
case $- in
  *i*)
    #{[} tmux
    # Run tmux on ssh connect
    # Source functions before this, so tmux is defined to pull options.
    if substrInStr "256" "$TERM" ; then
      TMUX_256_arg="-2"
    else
      TMUX_256_arg=""
    fi
    if [ -z "$TMUX_ALLOW_DETACH" ]; then
      execCmd="exec"
      # Else nothing
    fi
    if command -v tmux>/dev/null && [ -z "$NOTMUX" ]; then
      # Check HAVE_LOADED_BASH so that if you detach and bash gets upgraded,
      # you don't jump straight back into tmux.
      if [ -z "$TMUX" ] && [[ ! $TERM =~ screen ]] && \
        [ -z "$HAVE_LOADED_BASH" ]; then
              # PNAME="$(ps -o comm= $PPID)";
              # useTmuxFor="login sshd gnome-terminal init wslbridge-backe"
              # useTmuxFor="sshd"
              # if contains "$useTmuxFor" "$PNAME"; then
              if { [ -n "$SSHSESSION" ] || [ -z "$DISPLAY" ]; }; then
                # Hostname up to first .
                export HOSTNAME_BASE="${HOSTNAME%%.*}"
                export TMUX-STATUS-LEFT-LEN="$((22 + ${#HOSTNAME_BASE}))"
                # unset HAVE_LOADED_BASH PROFILE_LOADED
                [ -z "$TMUX_VERSION" ] && export TMUX_VERSION="$(tmux -V | cut -f2 -d' ')"
                if tmux ls 2> /dev/null | grep -q -v attached; then
                  $execCmd tmux $TMUX_256_arg attach -t $(tmux ls 2> /dev/null | grep -v attached | head -1 | cut -d : -f 1)
                else
                  $execCmd tmux $TMUX_256_arg
                fi
              fi
            fi
          fi
          unset TMUX_256_arg
          # {]} tmux

  # If available, replace bash with brew version. (More up-to-date than system.)
  # # # Only if running interactively. (Done in outer case statement, not
  # here.)
  if [ -n "$BASH" ] && [ -z "$HAVE_LOADED_BASH" ]; then
    if [ -n "$HOMEBREW_PREFIX" ]; then
      brewbash="$HOMEBREW_PREFIX/bin/bash"
      if [ "$BASH" == "$brewbash" ]; then
        export HAVE_LOADED_BASH=1
      elif [ -f "$brewbash" ]; then
        export SHELL="$brewbash"
        export HAVE_LOADED_BASH=1
        # Exec replaces the shell with the specified process.
        exec "$brewbash" -l
      fi
    fi
  fi


    # Register keys
    if [ -z "$SSH_AUTH_SOCK" ]; then
      # Decay after -t mins, or indefinitely.
      # eval $(ssh-agent -t 160m) > /dev/null # 2>&1
      eval $(ssh-agent) > /dev/null # 2>&1
    fi
    # Do this manually instead, using keybinding.
    # lastpass_ssh_key_add

    if [ -z "${BASE16_SHELL}" ]; then
      BASE16_SHELL="$HOME/.config/base16-shell/"
    fi
    if [ -d "${BASE16_SHELL}" ]; then
      [ -n "$PS1" ] && \
        [ -s "$BASE16_SHELL/profile_helper.sh" ] && \
        eval "$("$BASE16_SHELL/profile_helper.sh")"
    fi

    # {[} fzf
    if [ -f ~/.fzf.bash ]; then
      source ~/.fzf.bash
      export FZF_CTRL_R_OPTS='--sort'
      # Disable alt-c mapping to allow ;q to keep working.
      # Nah, just change ;q binding to ddi instead of cc
      # bind '"c":"c"
    fi
    # {]} fzf
    ;;
  *) return;;
esac
export PROFILE_LOADED=1
