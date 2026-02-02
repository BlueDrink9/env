#!/usr/bin/env sh
profile(){
# vim:ft=sh:tw=78:ts=2
# vim:foldmethod=marker:foldmarker={[},{]}
# Contains bash and cli bootstrapping/init code designed to be run once per
# shell
SCRIPT_DIR_LOCAL="$(realpath "$PROFILE_DIR/..")"
export DOTFILES_DIR=$(cd "${SCRIPT_DIR_LOCAL}/.." && pwd)
. "$SCRIPT_DIR_LOCAL/XDG_setup.sh"

if [ -n "$WSL_DISTRO_NAME" ] || command -v wslinfo > /dev/null 2>&1 || [ -f /proc/sys/fs/binfmt_misc/WSLInterop ]; then
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
  # if substrInStr "darwin1" "$OSTYPE"; then
  # parents="$(ps -o ppid,comm | grep $PPID)"
  parents="$(ps -p $PPID -o comm=)"
  # elif command -v pstree > /dev/null 2>&1; then
  #   parents="$(pstree -p | grep $PPID)"
  # else
  #   parents="$(ps | cut -d '' -f 2 | grep $PPID)"
  # fi
  if substrInStr "sshd" "$parents"; then
    SESSION_TYPE=remote/ssh
    export SSHSESSION=1
  fi
fi

# {[} Terminal-specific settings
# TODO use case
# Set defaults here for various terms
if substrInStr "kitty" "$TERM"; then
  COLORTERM="truecolor"
  USENF=${USENF:-1}
  TERM_PROGRAM="kitty"
elif [ -n "$ITERM_SESSION_ID" ]; then
  # Sets term prog, colourterm itself.
  USENF=${USENF:-1}
  # Default to dark, for dropdown (which is all I use iTerm for).
  COLOURSCHEME=${COLOURSCHEME:-solarized_dark}
elif substrInStr "Apple" "$TERM_PROGRAM"; then
  COLORTERM=16
elif substrInStr "screen" "$TERM"; then
  unset COLORTERM
elif substrInStr "Android" "$(uname -a)";  then
  # Termux
  export ISTERMUX=1
  if [ -z "$SSHSESSION" ]; then
    export ISTERMUXSCREEN=1
  fi
  export CLIP_PROGRAM_COPY="termux-clipboard-set"
  export CLIP_PROGRAM_PASTE="termux-clipboard-get"
  export HOSTNAME="$(getprop net.hostname)"
  export HOST="${HOSTNAME}"
  TERM_PROGRAM="termux"
  if command -v termux-ssh-askpass > /dev/null 2>&1; then
    export SSH_ASKPASS=termux-ssh-askpass
    export SSH_ASKPASS_REQUIRE='force'
  fi
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
if [ "$TERM_PROGRAM" = "vscode" ]; then
  COLORTERM="truecolor"
  export NOTMUX=1
fi

if [ -z "$HOMEBREW_PREFIX" ]; then
  case "${OSTYPE}" in
    (*darwin*)
      # OSX specific stuff
      export HOMEBREW_PREFIX="$HOME/homebrew"
      ;;(*linux*)
      # Linuxbrew paths
      export HOMEBREW_PREFIX="$HOME/.linuxbrew"
      export HOMEBREW_REPOSITORY="$HOMEBREW_PREFIX/Homebrew"
      ;;(*) true ;;
  esac
  if [ ! -d "$HOMEBREW_PREFIX" ]; then
    unset HOMEBREW_PREFIX
  fi
fi

# brew paths. Only before load to avoid loading twice.
# Only check first path entry ${PATH%%:*}, to ensure it is earlier than other things.
if [ -n "$HOMEBREW_PREFIX" ] && ! substrInStr "$HOMEBREW_PREFIX" "${PATH%%:*}" ; then
  export HOMEBREW_CELLAR="$HOMEBREW_PREFIX/Cellar"
  export PATH="$HOMEBREW_PREFIX/bin:$HOMEBREW_PREFIX/sbin:$PATH"
  export XDG_DATA_DIRS="$HOMEBREW_PREFIX/share:$XDG_DATA_DIRS"
  export MANPATH="$HOMEBREW_PREFIX/share/man:$MANPATH"
  export INFOPATH="$HOMEBREW_PREFIX/share/info:$INFOPATH"
fi
# Local (usually manually) installed packages. Should have highest priority.
if ! substrInStr "$HOME/.local" "${PATH%%:*}" ; then
  export PATH="$HOME/.local/bin:$HOME/.local/sbin:$PATH"
  export XDG_DATA_DIRS="$HOME/.local/share:$XDG_DATA_DIRS"
  export MANPATH="$HOME/.local/share/man:$MANPATH"
  export INFOPATH="$HOME/.local/share/info:$INFOPATH"
fi

binFolders="${SCRIPT_DIR_LOCAL}/scripts
$HOME/.cargo/bin
$HOME/.emacs.d/bin
$HOME/.local/npm-global/bin
$HOME/.local/share/nvim/mason/bin
"
while read -r folder; do
  if ! substrInStr "$folder" "${PATH}" ; then
    export PATH="$folder:$PATH"
  fi
done <<EOF
$binFolders
EOF

# Want xcode to be lower priority than brew
if substrInStr "darwin1" "$OSTYPE"; then
  xcodeBin="/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/"
  if ! substrInStr "${xcodeBin}" "${PATH}" ; then
    export PATH="${PATH}:${xcodeBin}"
  fi
  unset xcodeBin
fi

# Export path as a file for any scripts that might need it (e.g. sxhkd or
# bspwm)
echo "$PATH" >| "$XDG_CONFIG_HOME/.PATH"


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
    if command -v 'tmux'>/dev/null && [ -z "$NOTMUX" ]; then
      # Check HAVE_LOADED_BASH so that if you detach and bash gets upgraded,
      # you don't jump straight back into tmux.
      if [ -z "$TMUX" ] && ! substrInStr "screen" "$TERM" && \
        [ -z "$HAVE_LOADED_SHELL" ]; then
        # PNAME="$(ps -o comm= $PPID)";
        # useTmuxFor="login sshd gnome-terminal init wslbridge-backe"
        # if contains "$useTmuxFor" "$PNAME"; then
        if { [ -n "$SSHSESSION" ] || [ -z "$DISPLAY" ]; }; then
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

    # Not needed if system shell is sufficiently up to date.
    # But useful if homebrew shell version is desirable over system
    # version.
    # ensure_latest_shell

    # Replaced with ssh-ident (as ssh in shell/scripts)
    # ssh_agent_start

    ;;
  *) return;;
esac
export PROFILE_LOADED=1
}
profile
