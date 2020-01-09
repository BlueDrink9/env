bind_modechange(super_, "w", "manage")

bind([super_, alt], esc, "bspc quit")

#----------------------------------------
# focus/swap
#----------------------------------------

# focus the node in the given direction
# super + {_,shift + }{h,j,k,l}
#     bspc node -{f,s} {west,south,north,east}
keys={"h": "west", "j": "south", "k": "north", "l": "east"}
mods = {"": "-f", shift: "-s"}
for mod in mods:
    for key in keys:
        bind([super_, mod], key,
                "bspc node {} {}".format(mods[mod], keys[key]))
