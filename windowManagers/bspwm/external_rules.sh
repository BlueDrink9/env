#!/bin/sh
wid=$1
class=$2
instance=$3
consequences=$4
title="$(xdotool getwindowname "$wid")"

case "$title" in
  # All the little zoom windows shouldn't be captured, but the main ones
  # should be.
  # Main meeting window
  "*Zoom Meeting")
    eval "$consequences"
    [ "$state" ] || echo "state=tiled"
    ;;
  # Main application window, not in a meeting. Also meeting join windows.
  "*Zoom*")
    eval "$consequences"
    [ "$state" ] || echo "state=tiled"
    ;;
  # Misc side windows etc.
  "zoom")
    eval "$consequences"
    [ "$state" ] || echo "state=floating center=on follow=on border=off"
    ;;
esac

# if [ "$instance" = fontforge ] ; then
# 	title=$(xtitle "$wid")
# 	case "$title" in
# 		Layers|Tools|Warning)
# 			echo "focus=off"
# 			;;
# 	esac
# fi

# case "$class" in

	# zoom)
	# 	eval "$consequences"
	# 	[ "$state" ] || echo "state=pseudo_tiled"
	# 	;;

# esac
