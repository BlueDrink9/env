#!/bin/bash
# https://github.com/rajshekhar26/dotfiles/blob/master/.local/bin/floating_noborder

while read -r _ _ _ node state status; do
	[ "$state" = "floating" ] && [ "$status" == "on" ] && bspc config -n $node border_width 0
done < <(bspc subscribe node_state)
