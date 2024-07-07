#!/bin/sh

set -eu
set -o pipefail
# set -xo

# Read the xrandr output into a variable. Cut off the first line, which
# says how many monitors there are.
XRANDR_OUTPUT=$(xrandr --listmonitors | tail -n +2)

# Function to get the number of desktops for a monitor
get_desktop_count() {
  bspc query --desktops --monitor "$1" | wc -l
}

bspcdo(){
  bspc "$@"
}

# Temporary storage for other monitors
MONITORS_INFO=""

# Parse the xrandr output
while IFS= read -r line; do
  line=$(echo "$line" | xargs)
  MONITOR=$(echo "$line" | awk '{print $4}')
  IFS='+' read -r RESOLUTION POSITION <<EOF
  $(echo "$line" | awk '{print $3}')
EOF
  RESOLUTION=$(echo "$RESOLUTION" | sed 's|/[^x]*x|x|g; s|/[^x]*$||' | xargs)

  if echo "$line" | grep -q "\*"; then
    PRIMARY_MONITOR=$MONITOR
    PRIMARY_RESOLUTION=$RESOLUTION
    PRIMARY_POSITION=$POSITION
    PRIMARY_POSITION_H="${POSITION%%\+*}"
    PRIMARY_POSITION_V="${POSITION#*\+}"
  else
    MONITORS_INFO="${MONITORS_INFO}${MONITOR} ${RESOLUTION} ${POSITION}\n"
  fi
done <<< "$XRANDR_OUTPUT"

# Set up the primary monitor
if ! bspc query --monitors --names | grep -q "^$PRIMARY_MONITOR$"; then
  bspcdo wm --add-monitor "$PRIMARY_MONITOR" "$PRIMARY_RESOLUTION" "$PRIMARY_POSITION"
fi
if [ "$(get_desktop_count %"$PRIMARY_MONITOR")" -lt 4 ]; then
  bspcdo monitor %"$PRIMARY_MONITOR" --reset-desktops " " " " " " "4"
fi

# Initialize counters for left and right monitors
LEFT_COUNT=0
RIGHT_COUNT=0

# Assign desktops to other monitors
while IFS= read -r INFO; do
  MONITOR=$(echo "$INFO" | awk '{print $1}')
  RESOLUTION=$(echo "$INFO" | awk '{print $2}')
  POSITION=$(echo "$INFO" | awk '{print $3}')
  POSITION_H="${POSITION%%\+*}"
  POSITION_V="${POSITION#*\+}"

  if [ "$POSITION_H" -lt "$PRIMARY_POSITION_H" ]; then
    LEFT_COUNT=$((LEFT_COUNT + 1))
    DESKTOP_NAME=$(printf '%0.s<' $(seq 1 $LEFT_COUNT))
  else
    RIGHT_COUNT=$((RIGHT_COUNT + 1))
    DESKTOP_NAME=$(printf '%0.s>' $(seq 1 $RIGHT_COUNT))
  fi

  if ! bspc query --monitors --names | grep -q "^$MONITOR$"; then
    bspcdo wm --add-monitor "$MONITOR" "$RESOLUTION" "$POSITION"
  fi
  if [ "$(get_desktop_count %"$MONITOR")" -lt 2 ]; then
    bspcdo monitor %"$MONITOR" --reset-desktops "$DESKTOP_NAME" "$DESKTOP_NAME"
  fi
done <<< "$MONITORS_INFO" 
