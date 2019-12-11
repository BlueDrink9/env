import os
# import subprocess
import sys

def get_script_path():
    return os.path.dirname(os.path.realpath(sys.argv[0]))

cmd = engine.get_macro_arguments()[0]
if cmd is None:
    exit(1)

os.system(cmd)
    # retcode = subprocess.call(["bash", univi_sh, key])
    # output = subprocess.check_output(["bash", univi_sh, key]).decode()
    # engine.set_return_value(output)
