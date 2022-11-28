# Shared variables for window manager use.
export preselect_border_color="d75f5f" # pink-ish #d75f5f
export focused_border_color="eb7423"  # Orange #eb7423
export unfocused_border_color="1cb5db"  # cyan #1cb5db
export manage_border_color="00bc00"  # green #00bc00
export prefix_border_color="FAC863"  # mustard #FAC863
export window_resize_delta="100"

# Inspired by https://www.reddit.com/r/unixporn/comments/gfhons/bspwm_im_a_thief/
export focused_border_color="13CA91"  # Teal #13CA91
export unfocused_border_color="F808E9"  # purple #F808E9
# Picked from https://www.canva.com/colors/color-wheel/
export manage_border_color="0871F8"  # Blue #0871F8
# this is a nice blue colour #42A5F5
export manage_border_color="F88F08"  # Yellowy Orange #F88F08
export manage_border_color="ED3D09"  # Burnt Orange #ED3D09
export manage_border_color="08F817"  # Lime #08F817

# https://pinetools.com/split-complementary-colors
# Split complementary based on the above teal
export focused_border_color="13CA91"  # Teal #13CA91
export unfocused_border_color="ca12a7"  # Magenta-purple #ca12a7
export manage_border_color="ca3512"  # Burnt orange #ca3512

# # Split complementary based on the above purple
# export focused_border_color="07f88f"  # Teal #07f88f
# export unfocused_border_color="F808E9"  # purple
# export manage_border_color="70f807"  # Lime #70f807

# if [ -f "$HOME/.Xresources" ]; then
#     # Red
#     export focused_border_color=$(xrdb ~/.Xresources -query all | grep color9 | cut -f2)
#     # Cyan
#     export unfocused_border_color=$(xrdb ~/.Xresources -query all | grep color14 | cut -f2)
# fi
