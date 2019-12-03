#!/usr/bin/env bash

# SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

if substrInStr "xterm" "$TERM" || substrInStr "tmux" "$TERM" || [ "$TERM_PROGRAM" = "mintty" ]; then
  term_bar_cursor="\e[6 q"
  term_block_cursor="\e[1 q"
fi

shell="$(ps -p $$ | tail -1 | awk '{print $NF}')"
