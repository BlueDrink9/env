import os
import sys
import pathlib
from unittest import mock

engine = mock.MagicMock()
__file__ = os.path.realpath(sys.argv[0])

def get_script_dir():
    return os.path.dirname(os.path.realpath(sys.argv[0]))

script = get_script_dir() + "/bindings.py"
exec(pathlib.Path(script).read_text())
