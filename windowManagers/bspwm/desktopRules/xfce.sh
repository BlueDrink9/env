#! /bin/sh
# Whispermenu
bspc rule -a Wrapper-2.0 monitor=primary follow=on state=floating border=off rectangle=500x600+0+0
# Search
bspc rule -a Xfce4-appfinder follow=on state=floating border=off
bspc rule -a Thunar manage=on state=tiled border=on

get_xfce_panel_height(){
    fallback=35
    if command -v xfconf-query > /dev/null; then
      height="$(xfconf-query -c xfce4-panel -p /panels/panel-1/size -lv)"
      # Get everything after first space
      height=${height#* }
      # strip multiple spaces
      while [ "${height# }" != "$height" ]; do height=${height# }; done
    fi
    if [ -n "$height" ]; then
        echo "$height"
    else
        echo $fallback
    fi
}
export PANEL_HEIGHT=$(get_xfce_panel_height)
