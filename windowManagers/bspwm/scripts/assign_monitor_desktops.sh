#!/bin/sh
# Read the xrandr output into a variable. Cut off the first line, which
# says how many monitors there are.
XRANDR_OUTPUT=$(xrandr --listmonitors | tail -n +2)
BSPC_MONITOR_NAME_ID_MAP="$(paste \
  <(bspc query --monitors --names) <(bspc query --monitors) \
)"
printf "BSPC_MONITOR_NAME_ID_MAP:\n$BSPC_MONITOR_NAME_ID_MAP\n"
# "$BSPC_MONITOR_NAMES" 
# Initialize variables
PRIMARY_MONITOR=""
PRIMARY_RESOLUTION=""
PRIMARY_POSITION=""
MONITORS=()

# Function to get the number of desktops for a monitor
get_desktop_count() {
  bspc query --desktops --monitor "$1" | wc -l
}

# Get bspwm id for a monitor name
get_monitor_id(){
  lookup="$1"
  while IFS= read -r line; do
    # Split the line into key and value by tab
    key="${line%%$'\t'*}"
    value="${line#*$'\t'}"
    if [ "$lookup" = "$key" ]; then
      echo "$value"
      return
    fi
  done <<< "$BSPC_MONITOR_NAME_ID_MAP"
}

# Parse the xrandr output
while IFS= read -r line; do
  # Xargs to trim whitespace
  line="$(echo "$line" | xargs)"
  MONITOR=$(echo "$line" | awk '{print $4}')
  # Split the resolution and position string by the first plus
  IFS='+' read -r RESOLUTION POSITION <<< $(echo "$line" | awk '{print $3}')
  # Remove anything between a slash and an x, or the slash and the end of the split
  RESOLUTION=$(echo "$RESOLUTION" | sed 's|/[^x]*x|x|g; s|/[^x]*$||')

  if echo "$line" | grep -q "\*"; then
    PRIMARY_MONITOR=$MONITOR
    PRIMARY_MONITOR_ID=$(get_monitor_id $MONITOR)
    PRIMARY_RESOLUTION=$RESOLUTION
    PRIMARY_POSITION=$POSITION
    PRIMARY_POSITION_H="${POSITION%%\+*}"
    PRIMARY_POSITION_V="${POSITION#*\+}"
  else
    MONITORS+=("$MONITOR $RESOLUTION $POSITION")
  fi
done <<< "$XRANDR_OUTPUT"

# Set up the primary monitor
if ! echo "$(bspc query --monitors)" | grep -q "^$PRIMARY_MONITOR_ID$"; then
  echo bspc wm --add-monitor "$PRIMARY_MONITOR_ID" "$PRIMARY_RESOLUTION" "$PRIMARY_POSITION"
fi
if [ "$(get_desktop_count "$PRIMARY_MONITOR_ID")" -lt 4 ]; then
  echo bspc monitor "$PRIMARY_MONITOR_ID" --reset-desktops " " " " " " "4"
fi

# Initialize counters for left and right monitors
LEFT_COUNT=0
RIGHT_COUNT=0

# Assign desktops to other monitors
for MONITOR_INFO in "${MONITORS[@]}"; do
  MONITOR=$(echo "$MONITOR_INFO" | awk '{print $1}')
  MONITOR_ID=$(get_monitor_id $MONITOR)
  RESOLUTION=$(echo "$MONITOR_INFO" | awk '{print $2}')
  POSITION=$(echo "$MONITOR_INFO" | awk '{print $3}')
  POSITION_H="${POSITION%%\+*}"
  POSITION_V="${POSITION#*\+}"
  if [ "$POSITION_H" -lt "$PRIMARY_POSITION_H" ]; then
    LEFT_COUNT=$((LEFT_COUNT + 1))
    DESKTOP_NAME=$(printf '%0.s<' $(seq 1 $LEFT_COUNT))
  else
    RIGHT_COUNT=$((RIGHT_COUNT + 1))
    DESKTOP_NAME=$(printf '%0.s>' $(seq 1 $RIGHT_COUNT))
  fi
  if ! echo "$(bspc query --monitors)" | grep -q "^$MONITOR_ID$"; then
    echo bspc wm --add-monitor "$MONITOR_ID" "$RESOLUTION" "$POSITION"
  fi
  if [ "$(get_desktop_count "$MONITOR_ID")" -lt 2 ]; then
    echo bspc monitor "$MONITOR_ID" --reset-desktops "$DESKTOP_NAME" "$DESKTOP_NAME"
  fi
done
