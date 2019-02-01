# vim:ft=sh:tw=78:ts=2
# vim:foldmethod=marker:foldmarker={[},{]}
# Contains bash and cli bootstrapping/init code designed to be run once per
# shell

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
export DOTFILES_DIR=$(cd "${SCRIPT_DIR}/.." && pwd)

source "${SCRIPT_DIR}/functions.sh"

if [ -z "$HOMEBREW_PREFIX" ]; then
  if [[ "$OSTYPE" =~ "darwin1" ]]; then  # OSX specific stuff
    export HOMEBREW_PREFIX="$HOME/homebrew"
  elif [ "$OSTYPE" = "linux-gnu" ]; then  # Linux specific stuff
    # Linuxbrew paths
    export HOMEBREW_PREFIX="$HOME/.linuxbrew"
    export HOMEBREW_REPOSITORY="$HOMEBREW_PREFIX/Homebrew"
  fi
fi

# brew paths
if [ -n "$HOMEBREW_PREFIX" ]; then
  export HOMEBREW_CELLAR="$HOMEBREW_PREFIX/Cellar"
  export PATH="$HOMEBREW_PREFIX/bin:$HOMEBREW_PREFIX/sbin:$PATH"
  export XDG_DATA_DIRS="/$HOMEBREW_PREFIX/share:$XDG_DATA_DIRS"
  export MANPATH="$HOMEBREW_PREFIX/share/man:$MANPATH"
  export INFOPATH="$HOMEBREW_PREFIX/share/info:$INFOPATH"
fi

# Check if interactive is part of shell options (running interactively)
case $- in
  *i*)
    #{[} tmux
    # Run tmux on ssh connect
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
      if [[ ! $TERM =~ screen ]] && [[ -z $TMUX ]]; then
        # PNAME="$(ps -o comm= $PPID)";
        # useTmuxFor="login sshd gnome-terminal init wslbridge-backe"
        # useTmuxFor="sshd"
        # if contains "$useTmuxFor" "$PNAME"; then
        if { [ -n "$SSHSESSION" ] || [ -z "$DISPLAY" ]; }; then
          # unset HAVE_LOADED_BASH PROFILE_LOADED
          # echo LOADING tmux $HAVE_LOADED_BASH
          if tmux ls 2> /dev/null | grep -q -v attached; then
            $execCmd tmux $TMUX_256_arg attach -t $(tmux ls 2> /dev/null | grep -v attached | head -1 | cut -d : -f 1)
          else
            $execCmd tmux $TMUX_256_arg
          fi
        fi
      fi
    fi
    # {]} tmux

  # If available, replace bash with brew version. (More up-to-date than system.)
  # Only if running interactively.
    if [ -n "$BASH" ] && [ -z "$HAVE_LOADED_BASH" ]; then
      if [ -n "$HOMEBREW_PREFIX" ]; then
        brewbash="$HOMEBREW_PREFIX/bin/bash"
        if [ -f "$brewbash" ] && [ "$BASH" != "$brewbash" ]; then
          export HAVE_LOADED_BASH=1
          export SHELL="$brewbash"
          # Exec replaces the shell with the specified process.
          exec "$brewbash" -l
        fi
      fi
    fi


    # Register keys
    if [ -z "$SSH_AUTH_SOCK" ]; then
      # Decay after 120 mins
      eval $(ssh-agent -t 120m) > /dev/null # 2>&1
    fi
    lastpass_ssh_key_add

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
