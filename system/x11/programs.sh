#!/bin/sh
runWithLogging(){
  prog="$1"
  shift
  if command -v "$prog" > /dev/null 2>&1; then
    "$prog" $@ >| ~/.logs/"$prog".log 2>| ~/.logs/"$prog".err &
  else
    printf "Program '$1' not started. Not found." >&2
  fi
  unset prog
}

# Arbitrarily decide on Wellington
runWithLogging redshift-gtk -l -41.28664:174.77557 -t 6500:3000 -b 1:0.7
runWithLogging workrave
runWithLogging syncthing-gtk --minimized

# If running bspwm on its own, start other desktop elements.
if substrInStr "bspwm" "$DESKTOP_SESSION" || \
  [ -z "$DESKTOP_SESSION" -a "$DEFAULT_SESSION" = bspwm ] ; then
  # Desktop elements
  runWithLogging albert
  runWithLogging polybar -c ~/.config/polybar/config.ini mybar
  # Read by bspwm config
  export PANEL_HEIGHT=20
  runWithLogging dunst
  # latte-dock &
fi

# If started by startx, does not get set.
if [ -z "$DESKTOP_SESSION" ]; then
  # Usually bspwm, sometimes not. Adapts to what login manager asks for.
  # Requires dbus.
  exec $(get_session "$1")
fi

