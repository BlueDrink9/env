#!/bin/sh
runWithLogging(){
  if command -v "$1" > /dev/null 2>&1; then
    "$1" >| ~/.logs/"$1".log 2>| ~/.logs/"$1".err &
  else
    printf "Program '$1' not started. Not found." > &2
  fi
}

runWithLogging redshift-gtk
runWithLogging workrave
runWithLogging syncthing-gtk

# If running bspwm on its own, start other desktop elements.
if substrInStr "bspwm" "$DESKTOP_SESSION" || \
  [ -z "$DESKTOP_SESSION" -a "$DEFAULT_SESSION" = bspwm ] ; then
  # Desktop elements
  runWithLogging albert
  runWithLogging polybar -c ~/.config/polybar/config.ini mybar
  runWithLogging dunst
  # latte-dock &
fi

# If started by startx, does not get set.
if [ -z "$DESKTOP_SESSION" ]; then
  # Usually bspwm, sometimes not. Adapts to what login manager asks for.
  # Requires dbus.
  exec $(get_session "$1")
fi

