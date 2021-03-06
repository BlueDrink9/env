# Run autokey and start this script by running
# `autokey-run -s [/absolute/path/to/bindings.py]`.
import os
import sys
import pathlib

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
    # Escape brackets
    cmd = cmd.replace('>', '\\>')
    cmd = cmd.replace('<', '\\<')
    contents = "<system command='{}'>".format(cmd)
    create_phrase(name, contents, modifiers, key)

def multibind(mods, keys, command):
    if not isinstance(mods, dict):
        mods = {mods: ""}
    if not isinstance(keys, dict):
        keys = {keys: ""}
    for mod in mods:
        for key in keys:
            if isinstance(mod, tuple):
                modifiers = list(mod)
            else:
                modifiers = [mod]
            bind(modifiers, key,
                 command.format(mods[mod], keys[key]))


def bind_modechange(modifiers, key, mode):
    name = mode
    contents = "<script name='{}' args={}>".format(get_script_dir() + "/bindings.py", mode)
    create_phrase(name, contents, modifiers, key)


def create_phrase(name, contents, modifiers, key):
    if isinstance(modifiers, tuple):
        modifiers = list(modifiers)
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
    # print("{} + {} \n\t {}".format(modifiers, key, contents))
    assert(hotkey in folder.items)


def isMode(query):
    modes = ["default", "manage"]
    if query not in modes:
        print("Error: not a valid mode: {}".format(mode))
    return query == mode

def include(script):
    exec(pathlib.Path(script).read_text())

args = engine.get_macro_arguments()
if len(args) < 1:
    mode = "default"
else:
    mode = args[0]


# Prevent typing these wrong later.
ctrl="<ctrl>"
super_="<super>"
alt="<alt>"
shift="<shift>"
ctrlalt=(ctrl, alt)
ctrlshift=(ctrl, shift)
altshift=(alt, shift)
superctrl=(super_, ctrl)
superalt=(super_, alt)
supershift=(super_, shift)
superaltshift=(super_, alt, shift)
esc="<escape>"
backspace="<backspace>"
enter="<enter>"
space="<space>"
tab="<tab>"
up="<up>"
down="<down>"
left="<left>"
right="<right>"
noMod=""
# Button: The main key used for most bindings. Currently winkey/super.
# button=super_
# mode = store.get("mode", "normal")
# store.set_value("mode", "resize")


if isMode("default"):
    # Convert chunkwm colours by removing first two (alpha) values, and turning
    # 0x to #. Chunkwm uses #247dcc as focussed color.
    os.system('bspc config focused_border_color "#1cb5db"')
    include(get_script_dir() + "/default_bindings.py")

if isMode("manage"):
    os.system('bspc config focused_border_color "#00bc00"')
    include(get_script_dir() + "/manage_bindings.py")
