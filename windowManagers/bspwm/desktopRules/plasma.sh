#! /bin/sh

get_plasma_panel_height(){
    fallback=35
    config="$HOME/.config/plasmashellrc"
    if [ -f "$config" ]; then
        # Probably won't work for multiple panels, but it'll do for now
        lines=$(grep thickness "$config")
        height=$(echo "$lines" | cut -d "=" -f 2 | head -n1)
    fi
    if [ -n "$height" ]; then
        echo "$height"
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
bspc rule -a xwaylandvideobridge       state=floating, border=off
