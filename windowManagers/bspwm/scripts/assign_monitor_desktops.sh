#!/bin/sh

# Read the xrandr output into a variable
XRANDR_OUTPUT=$(xrandr)
BSPC_MONITORS=$(bspc query -M --names)

# Initialize variables
PRIMARY_MONITOR=""
PRIMARY_POSITION=0
MONITORS=()

# Function to get the number of desktops for a monitor
get_desktop_count() {
  bspc query -D -m "$1" | wc -l
}

# Parse the xrandr output
while IFS= read -r line; do
  if echo "$line" | grep -q " connected"; then
    MONITOR=$(echo "$line" | awk '{print $1}')
    RESOLUTION=$(echo "$line" | grep -o '[0-9]\+x[0-9]\++[0-9]\++[0-9]\+')
    POSITION=$(echo "$RESOLUTION" | sed 's/.*+\([0-9]\+\)+.*/\1/')
    if echo "$line" | grep -q " primary"; then
      PRIMARY_MONITOR=$MONITOR
      PRIMARY_POSITION=$POSITION
      if ! echo "$BSPC_MONITORS" | grep -q "^$PRIMARY_MONITOR$"; then
        bspc wm --add-monitor $PRIMARY_MONITOR $RESOLUTION
      fi
      if [ "$(get_desktop_count "$PRIMARY_MONITOR")" -lt 4 ]; then
        bspc monitor $PRIMARY_MONITOR --reset-desktops " " " " " " "4"
      fi
    else
      MONITORS+=("$POSITION $MONITOR $RESOLUTION")
    fi
  fi
done <<< "$XRANDR_OUTPUT"

# Sort the monitors by their position
IFS=$'\n' SORTED_MONITORS=($(sort -n <<<"${MONITORS[*]}"))
unset IFS

# Initialize counters for left and right monitors
LEFT_COUNT=0
RIGHT_COUNT=0

# Assign desktops to other monitors
for MONITOR_INFO in "${SORTED_MONITORS[@]}"; do
  POSITION=$(echo "$MONITOR_INFO" | awk '{print $1}')
  MONITOR=$(echo "$MONITOR_INFO" | awk '{print $2}')
  RESOLUTION=$(echo "$MONITOR_INFO" | awk '{print $3}')
  if [ "$POSITION" -lt "$PRIMARY_POSITION" ]; then
    LEFT_COUNT=$((LEFT_COUNT + 1))
    DESKTOP_NAME=$(printf '%0.s<' $(seq 1 $LEFT_COUNT))
  else
    RIGHT_COUNT=$((RIGHT_COUNT + 1))
    DESKTOP_NAME=$(printf '%0.s>' $(seq 1 $RIGHT_COUNT))
  fi
  if ! echo "$BSPC_MONITORS" | grep -q "^$MONITOR$"; then
    bspc wm --add-monitor $MONITOR $RESOLUTION
  fi
  if [ "$(get_desktop_count "$MONITOR")" -lt 2 ]; then
    bspc monitor $MONITOR --reset-desktops "$DESKTOP_NAME" "$DESKTOP_NAME"
  fi
done
