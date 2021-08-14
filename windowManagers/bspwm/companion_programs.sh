#! /bin/sh
SXHKD_STATUS_FIFO="/run/user/$UID/sxhkd.fifo"
if [ ! -e "${SXHKD_STATUS_FIFO}" ]; then
  mkfifo "${SXHKD_STATUS_FIFO}"
fi
sxhkd -m -1 -s "${SXHKD_STATUS_FIFO}" \
  -c "$HOME/.config/sxhkd/sxhkdrc" \
  "$DOTFILES_DIR"/windowManagers/bspwm/sxhkd/*.sxhkd \
  >| $HOME/.logs/sxhkd.log 2>| $HOME/.logs/sxhkd.err &
# Ensure reload config
pkill -USR1 -x sxhkd;

# Monitor sxhkd status, eg changing border colour during chains.
if ! pgrep -a sxhkd | grep sxhkd_status_mon.sh; then
  "$DOTFILES_DIR"/windowManagers/bspwm/scripts/sxhkd_status_mon.sh  "${SXHKD_STATUS_FIFO}" &
fi

wallpaper="$HOME/Pictures/wallpaper.jpg"
if [ -f "$wallpaper" ]; then
  feh --bg-fill "$wallpaper" &
fi
unset wallpaper

# -b starts as a bg process
picom --config "$DOTFILES_DIR"/desktop_elements/picom.conf --experimental-backends -b \
   >| $HOME/.logs/picom.log 2>| $HOME/.logs/picom.err

"$DOTFILES_DIR"/windowManagers/bspwm/scripts/floating_noborder.sh &

# Daemon tracks which media player has most recent activity, which ensures
# behavior acts on most expected player.
playerctld daemon

# flashfocus &
# pgrep autokey-gtk || autokey-gtk \
#    > $HOME/.logs/autokey.log 2> $HOME/.logs/autokey.err &
#   autokey-run -s "$DOTFILES_DIR"/windowManagers/bspwm/autokey/bindings.py
