#! /bin/sh
. "$DOTFILES_DIR"/windowManagers/variables.sh

[ $# -ne 1 ] && exit 1

status_fifo=$1

{ while read -r line ; do 
  msg=${line#?}
  prefix=${line:0:1}
  # See https://github.com/baskerville/sxhkd/blob/fe241b0d2d70c9c483b23cf3cd14f1383f0953a2/src/sxhkd.h#L38-L42 for all prefixes.
  case $prefix in
    B) # Begin chain
      bspc config focused_border_color "#$manage_border_color"
      ;;
    E) # End chain
      bspc config focused_border_color "#$focused_border_color"
      ;;
  esac
done } < "$status_fifo"
