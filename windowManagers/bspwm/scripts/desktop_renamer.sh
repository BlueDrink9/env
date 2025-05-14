#!/usr/bin/env bash
set -o pipefail

rename_desktops() {
  # set -x
  # Get monitor info
  XRANDR_OUTPUT=$(xrandr --listmonitors | tail -n +2)
  # declare -a MONITORS
  declare -A POSITIONS_H
  declare -A POSITIONS_V
  PRIMARY_MONITOR=""
  PRIMARY_POSITION_H=""
  PRIMARY_POSITION_V=""

  while IFS= read -r line; do
    line=$(echo "$line" | xargs)
    MON=$(echo "$line" | awk '{print $4}')
    POS=$(echo "$line" | awk '{print $3}' | cut -d'+' -f2)
    POS_H="${POS%%+*}"
    POS=$(echo "$line" | awk '{print $3}' | cut -d'+' -f3)
    POS_V="${POS%%+*}"

    # MONITORS+=("$MON")
    POSITIONS_H["$MON"]="$POS_H"
    POSITIONS_V["$MON"]="$POS_V"

    if echo "$line" | grep -q '\*'; then
      PRIMARY_MONITOR="$MON"
      PRIMARY_POSITION_H="$POS_H"
      PRIMARY_POSITION_V="$POS_V"
    fi
  done <<< "$XRANDR_OUTPUT"

  # MONITORS=($(bspc query --monitors --names))
  # MONITORS+=($(bspc query --monitors --names))
  # # Deduplicate monitors
  # # Remove entries unique to list 1
  # MONITORS=($(comm -1 <(printf '%s\n' "${MONITORS[@]}" | LC_ALL=C sort) <(printf '%s\n' "${BSPWM_MONITORS[@]}" | LC_ALL=C sort)))
  i=0
  for DESK in $(bspc query --desktops); do
    i=$((i + 1))
    # Skip the first 4 desktops
    if [ "$i" -lt 4 ]; then
      bspc desktop "$DESK" --rename " "
      continue
    fi
    MON="$(bspc query --monitors --desktop $DESK --names)"
    MON_POS_H="${POSITIONS_H["$MON"]}"
    MON_POS_V="${POSITIONS_V["$MON"]}"
    # TODO: track the leftmost and rightmost monitor found so far, use to increase number of arrows. Check assign_monitor_desktops.sh for how.
    if [ -n "$MON_POS_H" ]; then
      if [ "$MON" = "$PRIMARY_MONITOR" ]; then
        PREFIX=""
      elif [ "$MON_POS_H" -lt "$PRIMARY_POSITION_H" ]; then
        PREFIX="<"
      elif [ "$MON_POS_H" -gt "$PRIMARY_POSITION_H" ]; then
        PREFIX=">"
      else
        PREFIX=""
      fi
      if [ "$MON_POS_V" -lt "$PRIMARY_POSITION_V" ]; then
        PREFIX="${PREFIX}v"
      elif [ "$MON_POS_V" -gt "$PRIMARY_POSITION_V" ]; then
        PREFIX="${PREFIX}^"
      else
        PREFIX="${PREFIX}"
      fi
    else
      # Note that sometimes the monitors are out of sync, so the primary
      # monitor and the positions might not be the same as/available for the
      # bspwm monitors
      PREFIX="."
    fi
    bspc desktop "$DESK" --rename "${PREFIX}${i}"
  done
  # set +x
}

rename_desktops
bspc subscribe desktop_add desktop_remove desktop_swap desktop_transfer monitor_add monitor_remove monitor_swap | while read -r _; do
  # Avoid strange perceived delays adding/removing the desktop
  sleep 0.3
  rename_desktops
done

