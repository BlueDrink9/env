#!/bin/sh
# Reads xrandr and assigns desktops to each monitor, depending on location
# relative to primary.

# Read the xrandr output into a variable
XRANDR_OUTPUT=$(xrandr)

# Initialize variables
PRIMARY_MONITOR=""
PRIMARY_POSITION=0
MONITORS=()

# Parse the xrandr output
while IFS= read -r line; do
  if echo "$line" | grep -q " connected"; then
    MONITOR=$(echo "$line" | awk '{print $1}')
    RESOLUTION=$(echo "$line" | grep -o '[0-9]\+x[0-9]\++[0-9]\++[0-9]\+')
    POSITION=$(echo "$RESOLUTION" | sed 's/.*+\([0-9]\+\)+.*/\1/')
    if echo "$line" | grep -q " primary"; then
      PRIMARY_MONITOR=$MONITOR
      PRIMARY_POSITION=$POSITION
      "bspc wm -a $PRIMARY_MONITOR $RESOLUTION"
      "bspc monitor $PRIMARY_MONITOR -d \" \" \" \" \" \" \"4\""
    else
      MONITORS+=("$MONITOR $RESOLUTION $POSITION")
    fi
  fi
done <<< "$XRANDR_OUTPUT"

# Assign desktops to other monitors
for MONITOR_INFO in "${MONITORS[@]}"; do
  MONITOR=$(echo "$MONITOR_INFO" | awk '{print $1}')
  RESOLUTION=$(echo "$MONITOR_INFO" | awk '{print $2}')
  POSITION=$(echo "$MONITOR_INFO" | awk '{print $3}')
  if [ "$POSITION" -lt "$PRIMARY_POSITION" ]; then
    DESKTOP_NAME="<"
  else
    DESKTOP_NAME=">"
  fi
  "bspc wm -a $MONITOR $RESOLUTION"
  "bspc monitor $MONITOR -d \"$DESKTOP_NAME\" \"$DESKTOP_NAME\""
done
