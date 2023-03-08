import os
from pathlib import Path

scriptdir = Path(__file__).parent

config_files = [
    "ipython_bindings.py",
]
c.InteractiveShellApp.exec_files = [str(scriptdir / f) for f in config_files]


c.AliasManager.user_aliases = [
 ('la', 'ls -al')
]

## Autoindent IPython code entered interactively.
#  Default: True
c.InteractiveShell.autoindent = True

## Enable magic commands to be called without the leading %.
#  Default: True
c.InteractiveShell.automagic = True

## Options for displaying tab completions, 'column', 'multicolumn', and
#  'readlinelike'. These options are for `prompt_toolkit`, see `prompt_toolkit`
#  documentation for more information.
#  Choices: any of ['column', 'multicolumn', 'readlinelike']
#  Default: 'multicolumn'
c.TerminalInteractiveShell.display_completions = 'multicolumn'

from shutil import which
for editor in ["myVim", "nvim", "vim"]:
    if which(editor):
        c.TerminalInteractiveShell.editor = 'editor --cmd="let g:liteMode=1"'
## Display the current vi mode (when using vi editing mode).
c.TerminalInteractiveShell.prompt_includes_vi_mode = True

## Use 24bit colors instead of 256 colors in prompt highlighting.
if os.getenv("COLORTERM"):
    c.TerminalInteractiveShell.true_color = True


import sys
from operator import attrgetter
from prompt_toolkit.key_binding.vi_state import InputMode, ViState
import prompt_toolkit.key_binding.defaults as pt_defaults
from prompt_toolkit.filters.cli import ViInsertMode
from prompt_toolkit.keys import Keys

# Change cursor for different vi modes.
def set_input_mode(self, mode):
    shape = {InputMode.NAVIGATION: 1, InputMode.REPLACE: 3}.get(mode, 5)
    raw = u'\x1b[{} q'.format(shape)
    if hasattr(sys.stdout, '_cli'):
        out = sys.stdout._cli.output.write_raw
    else:
        out = sys.stdout.write
    out(raw)
    sys.stdout.flush()
    self._input_mode = mode

ViState._input_mode = InputMode.INSERT
ViState.input_mode = property(attrgetter('_input_mode'), set_input_mode)


c.TerminalInteractiveShell.editing_mode = 'vi'
