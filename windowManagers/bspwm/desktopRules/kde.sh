#! /bin/sh

get_plasma_panel_height(){
    fallback=35
    config="$HOME/.config/plasmashellrc"
    if [ -f "$config" ]; then
        # Probably won't work for multiple panels, but it'll do for now
        line=$(grep thickness "$config")
        height=$(echo "$line" | cut -d "=" -f 2 | head -n1)
        echo $height
    else
        echo $fallback
    fi
}
export PANEL_HEIGHT=$(get_plasma_panel_height)

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
bspc config bottom_padding $PANEL_HEIGHT

