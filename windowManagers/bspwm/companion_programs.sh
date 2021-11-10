#! /bin/sh
SXHKD_STATUS_FIFO="/run/user/${UID}/display${DISPLAY}/sxhkd.fifo"
if [ ! -e "${SXHKD_STATUS_FIFO}" ]; then
  mkdir -p "$(dirname "${SXHKD_STATUS_FIFO}")"
  mkfifo "${SXHKD_STATUS_FIFO}"
fi

if ! pgrep -a "^sxhkd$" | grep "${SXHKD_STATUS_FIFO}" > /dev/null 2>&1; then
  sxhkd -m -1 -s "${SXHKD_STATUS_FIFO}" \
    -c "$HOME/.config/sxhkd/sxhkdrc" \
    "$DOTFILES_DIR"/windowManagers/bspwm/sxhkd/*.sxhkd \
    >| $HOME/.logs/sxhkd.log 2>| $HOME/.logs/sxhkd.err &
else
  # Ensure reload config if already running
  pkill -USR1 -x sxhkd;
fi

# Monitor sxhkd status, eg changing border colour during chains.
# For some reason pgrep won't accept the last couple of letters?
if ! pgrep "sxhkd_status_mo?.??" > /dev/null 2>&1; then
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
command -v playerctld && playerctld daemon

# flashfocus &
# pgrep autokey-gtk || autokey-gtk \
#    > $HOME/.logs/autokey.log 2> $HOME/.logs/autokey.err &
#   autokey-run -s "$DOTFILES_DIR"/windowManagers/bspwm/autokey/bindings.py
