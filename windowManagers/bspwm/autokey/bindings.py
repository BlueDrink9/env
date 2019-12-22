# Add script to autokey, run autokey and start this script by running
# `autokey-run bindings default` (where 'bindings' is the script description in
# autokey.
import os
import sys

def get_script_dir():
    # Autokey runs the script via exec, so this doesn't work.
    # return os.path.realpath(sys.argv[0])
    # Might be able to use __file__, which gets overriden by
    # autokey.service.scriptrunner.execute but may not always work...
    return os.path.dirname(__file__)

# Creates if it doesn't exist, returns if it does.
# folder = engine.create_folder("bspwm_bindings", temporary=True)
folder = engine.create_folder("bspwm_bindings")
engine.remove_all_temporary(folder=folder)
engine.remove_all_temporary(folder=folder)
# Seems to be a bug with removing temporary from an individual folder.
engine.remove_all_temporary()
engine.remove_all_temporary()


def bind(modifiers, key, cmd):
    name = cmd
    system_cmd_path = get_script_dir() + "/" + "system_cmd.py"
    # Escape brackets
    cmd = cmd.replace('>', '\\>')
    cmd = cmd.replace('<', '\\<')
    contents = "<script name='{}' args='{}'>".format(system_cmd_path, cmd)
    create_phrase(name, contents, modifiers, key)


def bind_modechange(modifiers, key, mode):
    name = mode
    contents = "<script name='{}' args={}>".format(get_script_dir() + "/bindings.py", mode)
    create_phrase(name, contents, modifiers, key)


def create_phrase(name, contents, modifiers, key):
    if not isinstance(modifiers, list):
        modifiers = [modifiers]
    # Remove blanks that might be added automatically.
    try:
        modifiers.remove("")
    except ValueError:
        pass
        # hotkey = ([engine.Key.SHIFT])
    hotkey = engine.create_phrase(folder, name, contents, hotkey=(modifiers, key),
                         temporary = True)
    assert(hotkey in folder.items)


def isMode(query):
    modes = ["default", "manage"]
    if query not in modes:
        print("Error: not a valid mode: {}".format(mode))
    return query == mode

args = engine.get_macro_arguments()
if len(args) < 1:
    mode = "default"
else:
    mode = args[0]
print(mode)

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

# Convert chunkwm colours by removing first two (alpha) values, and turning 0x
# to #.
if isMode("default"):
    os.system('bspc config focused_border_color "#247dcc"')
    bind_modechange(super_, "w", "manage")

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


if isMode("manage"):
    os.system('bspc config focused_border_color "#00bc00"')
    bind_modechange(super_, "w", "default")
