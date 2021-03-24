# Shared variables for window manager use.
export preselect_border_color="d75f5f" # pink-ish
export focused_border_color="eb7423"  # Orange
export unfocused_border_color="1cb5db"  # cyan
export manage_border_color="00bc00"  # green
export prefix_border_color="FAC863"  # mustard
export window_resize_delta="100"

# Red
if [ -f "$HOME/.Xresources" ]; then
    export focused_border_color=$(xrdb ~/.Xresources -query all | grep color1 | cut -f2)
    # Cyan
    export unfocused_border_color=$(xrdb ~/.Xresources -query all | grep color6 | cut -f2)
fi
