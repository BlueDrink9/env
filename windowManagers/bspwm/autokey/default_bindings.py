bind_modechange(super_, "w", "manage")

bind([super_, alt], esc, "bspc quit")

#----------------------------------------
# focus/swap
#----------------------------------------

# focus the node in the given direction
# super + {_,shift + }{h,j,k,l}
#     bspc node -{f,s} {west,south,north,east}
keys={"h": "west", "j": "south", "k": "north", "l": "east"}
multibind(super_, keys, "bspc node -f {} {}")

# Normally duplicated by window manager
# focus the next/previous node in the current desktop
multibind({alt: 'next', altshift: 'prev'}, tab, "bspc node -f {}.local {}")
# focus the older or newer node in the focus history
# super + {o,i}
multibind({super_: 'newer', supershift: 'older'}, tab, \
          "bspc wm -h off; bspc node {} -f; bspc wm -h on {}")

# close and kill
multibind({super_: "-c", supershift: "-k"}, {"x":""}, "bspc node {}{}")

# Minimize/hide
# super - z : osascript -e 'tell application "System Events" to keystroke "m" using {command down}'

# #
# # Desktops
# #

# focus the next/previous desktop in the current monitor
# Destroys the old one if it is empty first.
# See https://blog.codezen.org/2015/03/13/on-bspwm-tweaking/ #Named Desktops to see how to create if no new one found.
multibind(super_, {"[": 'prev', "]": 'next'}, "bspc desktop -f {}{}.local")
multibind(supershift, {"[": 'prev', "]": 'next'}, "bspc node --to-desktop {}{}.local")
# Move to desktop and focus on it.
# Double {} for literal {}.
move = 'id=$(bspc query --nodes --node); bspc node --to-desktop {}; bspc desktop --focus {}; ' + 'bspc node --focus ${{id}}'
bind(superctrl, "[", move.format("prev", "prev"))
bind(superctrl, "]", move.format("next", "next"))

# Move to specified desktop
# super + {_,shift + }{1-9,0}
#	bspc {desktop -f,node -d} '^{1-9,10}'

# Create a new desktop named ' '
bind(super_, "n", "bspc monitor --add-desktops ' '")
# Delete current desktop
bind(supershift, "n", "bspc desktop -r -f prev")


# Pseudo-minimize (to desktop "O")
minimize = '''
  D=$(bspc query –desktops –desktop focused);
  W=$(bspc query –windows –window focused);
  bspc query –desktops | grep -q "$D" || bspc monitor –add-desktops $D;
  bspc window $W --to-desktop $D
'''
bind(super_, 'z', minimize)
# Restore all minimized windows from this desktop
restore = """
  D=$(bspc query –desktops –desktop focused);
  if bspc query –desktops | grep -q "$D"; then
    for i in $(bspc query –windows –desktop $D); do
      bspc window $i –to-desktop $D;
      bspc window $i –focus;
      bspc window –swap biggest;
    done;
    bspc desktop $D –remove;
  fi
"""
bind(supershift, 'z', restore)
