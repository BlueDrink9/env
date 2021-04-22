# Shared variables for window manager use.
export preselect_border_color="d75f5f" # pink-ish
export focused_border_color="eb7423"  # Orange
export unfocused_border_color="1cb5db"  # cyan
export manage_border_color="00bc00"  # green
export prefix_border_color="FAC863"  # mustard
export window_resize_delta="100"

# Inspired by https://www.reddit.com/r/unixporn/comments/gfhons/bspwm_im_a_thief/
export focused_border_color="13CA91"  # Teal
export unfocused_border_color="F808E9"  # purple
# Picked from https://www.canva.com/colors/color-wheel/
export manage_border_color="0871F8"  # Blue
export manage_border_color="F88F08"  # Yellowy Orange
export manage_border_color="ED3D09"  # Burnt Orange
export manage_border_color="08F817"  # Lime

# https://pinetools.com/split-complementary-colors
# Split complementary based on the above teal
export focused_border_color="13CA91"  # Teal
export unfocused_border_color="ca12a7"  # Magenta-purple
export manage_border_color="ca3512"  # Burnt orange

# Split complementary based on the above purple
export focused_border_color="07f88f"  # Teal
export unfocused_border_color="F808E9"  # purple
export manage_border_color="70f807"  # Lime

# if [ -f "$HOME/.Xresources" ]; then
#     # Red
#     export focused_border_color=$(xrdb ~/.Xresources -query all | grep color9 | cut -f2)
#     # Cyan
#     export unfocused_border_color=$(xrdb ~/.Xresources -query all | grep color14 | cut -f2)
# fi
