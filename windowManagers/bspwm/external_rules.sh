#!/bin/sh
wid=$1
class=$2
instance=$3
initial_consequences=$4

exec 2> ~/.logs/bspwm_external_rules.err

# printf "wid=$1 class=$2 instance=$3 initial_consequences=$4" >&2

# Only run 'expensive' xdotool checks if needed
if [ "$class" = "zoom" ]; then
  title="$(xdotool getwindowname "$wid")"
  case "$title" in
    # All the little zoom windows shouldn't be captured, but the main ones
    # should be.
    # Main meeting window
    "*Zoom Meeting")
      eval "$initial_consequences"
      echo "state=tiled"
    ;;
    # Main application window, not in a meeting. Also meeting join windows.
    "*Zoom*")
      eval "$initial_consequences"
      echo "state=tiled"
    ;;
    # Misc side windows etc.
    "zoom")
      eval "$initial_consequences"
      echo "state=floating center=on follow=on border=off focus=off"
      # zoom's main window will start titled 'zoom' initially, then change.
      # So for 'zoom' windows, wait a second after setting state just in case
      # the window is actually that first one.
      # https://www.reddit.com/r/i3wm/comments/p0fdxa
      if ! command -v xprop > /dev/null || \
        ! command -v timeout > /dev/null; then
        return
      fi
      # Monitor the Zoom window for title changes using xprop
      timeout 0.3s xprop -id "$wid" -spy _NET_WM_NAME | while read -r line; do
        # Remove prefix up to and including the first double quote
        title="${line#*\"}"
        # Remove suffix starting from the last double quote
        title="${title%\"*}"
        if [[ "$title" == Zoom* ]]; then
          echo "state=tiled center=off follow=on focus=on border=off"
          break
        fi
      done
  ;;
  esac
fi

case "$(xprop -id $wid "_NET_WM_STATE")" in 
  # I think this will get misc "above" windows. At the very least, it
  # should for filebot.
  "*_NET_WM_STATE_ABOVE*")
    echo "state=floating center=on follow=on border=off focus=off"
    break
    ;;
esac

# Only run expensive xprop checks for plasmashell
if [ "$class" = "plasmashell" ]; then
    # fetch the window type from X properties
    case "$(xprop -id "$wid" _NET_WM_WINDOW_TYPE)" in
      *"_NET_WM_WINDOW_TYPE_DOCK"*)
        # This is the taskbar/panel
        # Force it to be normal layer and managed, so that monacle windows cover it rather than it persistently floating above.
        echo "state=floating border=off manage=on sticky=on focus=off layer=normal follow=off private=on"
        ;;
      *)
        # This is a popup (calendar, application launcher, volume)
        # Keep it floating, but let it stay on the normal layer (on top of windows)
        echo "state=floating border=off center=off"
        ;;
    esac
fi
