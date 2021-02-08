#!/usr/bin/env bash

# SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

export DOOMDIR="$DOTFILES_DIR/editors/emacs/doom.d"

if substrInStr "xterm" "$TERM" || substrInStr "tmux" "$TERM" || [ "$TERM_PROGRAM" = "mintty" ]; then
  term_bar_cursor="\e[6 q"
  term_block_cursor="\e[1 q"
fi
