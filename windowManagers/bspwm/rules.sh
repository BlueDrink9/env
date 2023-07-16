# Rules
bspc rule -a Skype            pseudo_tiled=on
# bspc rule -a Spotify pseudo_tiled=on
# bspc rule -a Thunar pseudo_tiled=on
bspc rule -a Chromium         desktop='^2'
# bspc rule -a xdm state=floating
# This is the actual xdm WM_CLASS.
bspc rule -a java-lang-Thread state=floating
bspc rule -a mplayer2         state=floating
bspc rule -a Kupfer.py        focus=on
bspc rule -a Screenkey        manage=off   floating=on
bspc rule -a File-roller      pseudo_tiled=on
bspc rule -a Transmission-gtk pseudo_tiled=on
bspc rule -a Conky            sticky=on    manage=off     lower=on
bspc rule -a astime           sticky=on
bspc rule -a feh              floating=on  manage=off
# Anbox onenote. Doesn't resize well, so tiling is a bad idea.
bspc rule -a onenote          floating=on
bspc rule -a anbox            floating=on
bspc rule -a Gimp             desktop='^8' state=floating follow=on
# Emacs needs to be forced to start tiling.
bspc rule -a Emacs            state=tiled
bspc rule -a albert           floating=on  border=off     focus=on
bspc rule -a kitty-dropdown   sticky=on
# All the little zoom windows don't need to be captured, but the main ones should be.
bspc rule -a zoom state=floating center=on follow=on border=off
# Can only differentiate with wm_name
bspc rule -a '*:Zoom Meeting' state=tiling border=on follow=off
bspc rule -a '*:Zoom -*' state=tiling border=on follow=off
# bspc rule -a <zoom\_notification\_class>

# _NET_WM_STATE(ATOM) = _NET_WM_STATE_ABOVE, _NET_WM_STATE_STAYS_ON_TOP
# bspc rule -a zoom manage=off floating=on
if [ "$XDG_SESSION_DESKTOP" = "plasmax11" ] || \
    [ "$XDG_SESSION_DESKTOP" = "KDE" ] || \
    [ "$XDG_CURRENT_DESKTOP" = "KDE" ]
then
    . "$DOTFILES_DIR"/windowManagers/bspwm/desktopRules/plasma.sh
fi
