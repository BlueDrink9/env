# Add script to autokey, run autokey and start this script by running
# `autokey-run bindings default` (where 'bindings' is the script description in
# autokey.
import os
import sys

def get_script_path():
    return os.path.dirname(os.path.realpath(sys.argv[0]))

# Creates if it doesn't exist, returns if it does.
folder = engine.create_folder("bspwm_bindings", temporary=True)
engine.remove_all_temporary(folder="bspwm_bindings")

def bind(modifiers, key, cmd):
    # Remove blanks that might be added automatically.
    try:
        modifiers.remove("")
    except ValueError:
        pass
    name = cmd
    contents = "<script name=system_cmd args={}>".format(cmd)
    abbreviations = key
        # hotkey = ([engine.Key.SHIFT])
    engine.create_phrase(folder, name, contents, abbreviations, modifiers, temporary = True)


mode = engine.get_macro_arguments()[0]
try:
    mode
except NameError:
    mode = "default"
if mode is None:
    mode = "default"

# Prevent typing these wrong later.
ctrl="<ctrl>"
super_="<super>"
alt="<alt>"
shift="<shift>"
esc="<escape>"
backspace="<backspace>"
enter="<enter>"
space="<space>"
tab="<tab>"
up="<up>"
down="<down>"
left="<left>"
right="<right>"
# Button: The main key used for most bindings. Currently winkey/super.
button=super_
# mode = store.get("mode", "normal")
# store.set_value("mode", "resize")

if mode == "default":
    bind([button, alt], esc, "bspc quit")

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
            bind([button, mod], key,
                    "bspc node {} {}".format(mods[mod], keys[key]))
