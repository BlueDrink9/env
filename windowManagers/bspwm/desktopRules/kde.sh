#! /bin/sh

bspc rule -a plasmashell    border=off, state=floating, center=off
bspc rule -a plasma-desktop state=floating, border=off
bspc rule -a Plasma         state=floating, border=off
bspc rule -a win7           state=floating, border=off
bspc rule -a krunner        state=floating, border=off
bspc rule -a Kmix           state=floating, border=off
bspc rule -a Klipper        state=floating, border=off
bspc rule -a Plasmoidviewer state=floating, border=off
bspc rule -a mplayer2       state=floating, border=off


# bspc config right_padding 0
# bspc config left_padding 0
# bspc config top_padding 0
# Enough to avoid taskbar
bspc config bottom_padding 25

