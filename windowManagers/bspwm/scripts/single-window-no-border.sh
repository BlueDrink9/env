#!/bin/sh
bspc control --subscribe | while read line; do
  W=$(bspc query –desktop focused –windows | wc -l)
  if [ "$W" = 1 ]; then
    # This isn't correct. Needs to be a rule. Maybe just set width 0, then back?
    bspc config border=off
  else
    bspc config border=on
  fi
done
