#!/bin/sh
# Keyboard setup stuff
export XKB_DEFAULT_LAYOUT="us,us"
export XKB_DEFAULT_VARIANT="colemak,"
export XKB_DEFAULT_OPTIONS="grp:alt_shift_toggle,caps:backspace,grp_led:caps,altwin:swap_alt_win"
capsToBS="-option caps:backspace"
altWinSwap="-option altwin:swap_alt_win"
winSpaceToggle="-option grp:win_space_toggle"
capsLed="-option grp_led:caps"
colemak="-layout 'us, us' -variant 'colemak,'"
eval "setxkbmap $colemak $capsToBS $altWinSwap $winSpaceToggle $capsLed"
unset colemak capsToBS altWinSwap winSpaceToggle capsLed

# Don't use wierd x pointer when no active window.
xsetroot -cursor_name left_ptr

# Set key repeat rate. Delay 280 milisecond, 30 per sec.
xset r rate 280 30
# Enable capslock key repeat (to allow repeating it as backspace).
xset r 66
