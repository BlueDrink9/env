#!/bin/sh

# Reuse existing colors defined as Xresources
XcolourFile="$1"
_SEDCMD='s/.*\*color\([0-9]\{1,\}\).*#\([0-9a-fA-F]\{6\}\).*/\1 \2/p'
for i in $(sed -n "$_SEDCMD" "${XcolourFile}" | awk '$1 < 16 {printf "\\e]P%X%s", $1, $2}'); do
    echo -en "$i"
done
clear
