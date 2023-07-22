#!/bin/sh
wid=$1
class=$2
instance=$3
consequences=$4
title="$(xdotool getwindowname "$wid")"

# if [ "$title" = "Zoom Meeting" ] ; then

case "$title" in
  "Zoom Meeting")
    eval "$consequences"
    [ "$state" ] || echo "state=tiled"
    ;;

# 	Layers|Tools|Warning)
  # 		echo "focus=off"
  # 		;;
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
