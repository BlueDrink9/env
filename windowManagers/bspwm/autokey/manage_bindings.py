bind_modechange(super_, "w", "default")
bind_modechange([], enter, "default")
bind_modechange([], esc, "default")

# make sxhkd reload its configuration files.
# This isn't really necessary for autokey, because changing modes updates the
# keys anyway.
# bind(shift, 'c', "pkill -USR1 -x sxhkd")

# focus or swap node in the given direction
keys={"h": "west", "j": "south", "k": "north", "l": "east"}
mods = {super_: "-f", supershift: "-s"}
multibind(mods, keys, "bspc node {} {}")

# Move/warp window (not implemented in bspwm?)

# focus the node for the given path jump
# This has a duplicate with a plasma shortcut, I think
# super + {p,b,comma,period}
#	bspc node -f @{parent,brother,first,second}


# #
# # move/resize
# #

# # expand a window by moving one of its side outward
# # Semicolon makes this a prefix, colon makes it a mode (leave with esc).
keys={left:"left -20 0",down:"bottom 0 20",up:"top 0 -20",right:"right 20 0"}
multibind(shift, keys, "bspc node -z {}{}")
# # contract a window by moving one of its side inward
keys={left:"right -20 0",down:"top 0 20",up:"bottom 0 -20",right:"left 20 0"}
multibind(ctrlshift, keys, "bspc node -z {}{}")

# # move a floating window
keys={left:"-20 0",down:"0 20",up:"0 -20",right:"20 0"}
multibind(noMod, keys, "bspc node -v {}{}")

# #
# # states
# #

# close and kill
mods = {super_: "-c", supershift: "-k"}
multibind(mods, 'x', "bspc node {}{}")

# set the window state. Floating is a toggle, needs extra quotes.
keys={'t': "tiled", 'u':"'~floating'", 'm':"fullscreen"}
multibind(shift, keys, "bspc node {}{}")
bind([shift, ctrl], 't', "bspc node -t pseudo_tiled")
# alternate between the tiled and monocle layout
bind([shift], 'm', "bspc desktop -l next")
# Set the node flags.
# p for pin
# Don't know what the rest of these flags do.
keys={'m': "marked", 'x':"locked", 'p':"sticky", 'z':'private'}
multibind(ctrl, keys, "bspc node -g {}{}")

# Hide and unhide nodes?
keys = {"<minus>": "-g hidden", "<plus>": "$(bspc query -N -n .hidden | tail -n1) -g hidden=off"}
multibind(noMod, keys, "bspc node {}{}")


# #
# # Desktops
# #

# shift: Swap desktops within current monitor
# ctrl: Move desktop to different monitor
multibind({shift: "-s", ctrlalt: "-m"}, {"[": 'prev', "]": 'next'}, "bspc desktop {} {}")

# Just numbers is used to launch taskbar shortcuts.
# focus or send to the given desktop
# super + shift {1-9,0}
#	bspc node -d '^{1-9,10}'

# focus the last node/desktop
# super + {grave,Tab}
#	bspc {node,desktop} -f last

# send the newest marked node to the newest preselected node
# super + y
# 	bspc node newest.marked.local -n newest.!automatic.local
# swap the current node and the biggest node
# super + g
# 	bspc node -s biggest

# Create a new desktop named ' '
bind(noMod, "n", "bspc monitor --add-desktops ' '")
# Delete current desktop
bind(shift, "n", "bspc desktop -r -f prev")

#----------------------------------------
# Manipulations
#----------------------------------------

# Rotate desktop
mods = {noMod:"90", shift:"-90"}
multibind(mods, 'r', "bspc node @/ --rotate {}{}")
# Make split ratios equal
bind('', '=', "bspc node @/ --equalize")
# Make split ratios balanced.
bind('', '-', "bspc node @/ --balance")

#
# preselect
#

# preselect the direction
keys={"h": "west", "j": "south", "k": "north", "l": "east"}
multibind(noMod, keys, "bspc node -p {}{}")
# cancel the preselection for the focused node
bind(ctrl, 'c', "bspc node -p cancel")
# cancel the preselection for the focused desktop
# super + ctrl + shift + space
bind(ctrlshift, 'c', \
     "bspc query -N -d | xargs -I id -n 1 bspc node id -p cancel")


# preselect the ratio
# keys={"h": "west", "j": "south", "k": "north", "l": "east"}
# bind(noMod, keys, "bspc node -p {}")
# super + ctrl + {1-9}
#	bspc node -o 0.{1-9}

# Save layout using
# https://github.com/johannesjo/linux-window-session-manager
bind(noMod, 'w', "lwsm save bspwm")
bind(noMod, 'e', "lwsm restore bspwm")
