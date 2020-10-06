#!/bin/sh

# Used by yabai and skhd
# Open kitty window in the current space and display instead of the space and
# display where the first kitty window was created.
display=$(yabai -m query --displays --display | jq .index)
space=$(yabai -m query --spaces --space | jq .index)

yabai -m signal --add event=window_created action=" \
  yabai -m signal --remove 'testkitty' &&
  yabai -m window $YABAI_WINDOW_ID --display $display &&
  yabai -m space --focus $space &&
  yabai -m display --focus $display" \
  app="kitty" label="testkitty"

$HOME/Applications/kitty.app/Contents/MacOS/kitty --single-instance || /Applications/kitty.app/Contents/MacOS/kitty --single-instance
