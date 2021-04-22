#! /bin/sh
SXHKD_STATUS_FIFO="/run/user/$UID/sxhkd.fifo"
if [ ! -f "${SXHKD_STATUS_FIFO}" ]; then
  mkfifo "${SXHKD_STATUS_FIFO}"
fi
pgrep sxhkd || \
  sxhkd -m -1 -s "${SXHKD_STATUS_FIFO}" \
  -c "$HOME/.config/sxhkd/sxhkdrc" \
  "$DOTFILES_DIR"/windowManagers/bspwm/sxhkd/*.sxhkd \
  >| $HOME/.logs/sxhkd.log 2>| $HOME/.logs/sxhkd.err &

wallpaper="$HOME/Pictures/wallpaper.jpg"
if [ -f "$wallpaper" ]; then
  feh --bg-fill "$wallpaper" &
fi
unset wallpaper

# -b starts as a bg process
picom --config "$DOTFILES_DIR"/desktop_elements/picom.conf --experimental-backends -b \
   >| $HOME/.logs/picom.log 2>| $HOME/.logs/picom.err

"$DOTFILES_DIR"/windowManagers/bspwm/scripts/floating_noborder.sh &

# flashfocus &
# pgrep autokey-gtk || autokey-gtk \
#    > $HOME/.logs/autokey.log 2> $HOME/.logs/autokey.err &
#   autokey-run -s "$DOTFILES_DIR"/windowManagers/bspwm/autokey/bindings.py
