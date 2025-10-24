#! /bin/sh
sh "$DOTFILES_DIR"/shell/scripts/sxhkd_mine
sh "$DOTFILES_DIR"/shell/scripts/picom_mine

wallpaper="$HOME/Pictures/wallpaper.jpg"
if [ -f "$wallpaper" ]; then
  feh --bg-fill "$wallpaper" &
fi
unset wallpaper

# # Plasma's nightshift builtin nightshift requires kwin so still need redshift.
# Disabled for now because home manager handles it
# if ! pgrep "redshift" > /dev/null 2>&1; then
#   redshift-gtk -l -41.28664:174.77557 -t 6500:3000 -b 1:0.7 &
# fi

# Stores it own logs, so don't need any in .logs
talon > /dev/null &

"$DOTFILES_DIR"/windowManagers/bspwm/scripts/floating_noborder.sh &

# Daemon tracks which media player has most recent activity, which ensures
# behavior acts on most expected player.
command -v playerctld >/dev/null 2>&1 && playerctld daemon >/dev/null 2>&1

command -v xremap >/dev/null 2>&1 && \
  xremap "$DOTFILES_DIR"/misc/xremap.yml \
  >| $HOME/.logs/xremap.log 2>| $HOME/.logs/xremap.err &

# flashfocus &
# pgrep autokey-gtk || autokey-gtk \
#    > $HOME/.logs/autokey.log 2> $HOME/.logs/autokey.err &
#   autokey-run -s "$DOTFILES_DIR"/windowManagers/bspwm/autokey/bindings.py
