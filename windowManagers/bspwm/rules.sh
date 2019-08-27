# Rules
bspc rule -a Skype pseudo_tiled=on
# bspc rule -a Spotify pseudo_tiled=on
# bspc rule -a Thunar pseudo_tiled=on
bspc rule -a Chromium desktop='^2'
# bspc rule -a xdm state=floating
# This is the actual xdm WM_CLASS.
bspc rule -a java-lang-Thread state=floating
bspc rule -a mplayer2 state=floating
bspc rule -a Kupfer.py focus=on
bspc rule -a Screenkey manage=off floating=on
bspc rule -a File-roller pseudo_tiled=on
bspc rule -a Transmission-gtk pseudo_tiled=on
bspc rule -a Conky sticky=on manage=off lower=on
bspc rule -a astime sticky=on
bspc rule -a feh floating=on manage=off
bspc rule -a onenote floating=on
bspc rule -a anbox floating=on
bspc rule -a Gimp desktop='^8' state=floating follow=on
