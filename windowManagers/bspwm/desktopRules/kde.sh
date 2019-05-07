#! /bin/sh

bspc rule -a Plasma state=floating
bspc rule -a plasmashell state=floating
bspc rule -a "Plasma" state=floating
bspc rule -a "plasmashell" state=floating
bspc rule -a "plasmashell", "plasmashell" state=floating
bspc rule -a "Desktop — Plasma" state=floating
bspc rule -a plasma-desktop state=floating
bspc rule -a win7 state=floating
bspc rule -a krunner state=floating
bspc rule -a Kmix state=floating
bspc rule -a Klipper state=floating
bspc rule -a Plasmoidviewer state=floating
bspc rule -a mplayer2 state=floating


# bspc config right_padding 0
# bspc config left_padding 0
# bspc config top_padding 0
bspc config bottom_padding 25

