import os
from pathlib import Path
from sys import stdout
from operator import attrgetter
from prompt_toolkit.key_binding.vi_state import InputMode, ViState
from shutil import which

c.BaseIPythonApplication.extra_config_file = ""


scriptdir = Path(__file__).parent

config_files = [
    "bindings.py",
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
c.TerminalInteractiveShell.space_for_menu = 6

c.TerminalInteractiveShell.wildcards_case_sensitive = False

for editor in ["myVim", "nvim", "vim"]:
    if which(editor):
        c.TerminalInteractiveShell.editor = f'{editor} --cmd "let g:liteMode=1"'


## Use 24bit colors instead of 256 colors in prompt highlighting.
if os.getenv("COLORTERM"):
    c.TerminalInteractiveShell.true_color = True


if hasattr(c.TerminalInteractiveShell, "timeoutlen"):
    c.TerminalInteractiveShell.timeoutlen = 0.3

c.TerminalInteractiveShell.prompt_includes_vi_mode = True


# Change cursor for different vi modes.
def set_input_mode(self, mode):
    shape = {InputMode.NAVIGATION: 1, InputMode.REPLACE: 3}.get(mode, 5)
    raw = f'\x1b[{shape} q'
    if hasattr(stdout, '_cli'):
        out = stdout._cli.output.write_raw
    else:
        out = stdout.write
    out(raw)
    stdout.flush()
    self._input_mode = mode

ViState._input_mode = InputMode.INSERT
ViState.input_mode = property(attrgetter('_input_mode'), set_input_mode)



c.TerminalInteractiveShell.editing_mode = 'vi'
if hasattr(c.TerminalInteractiveShell, "emacs_bindings_in_vi_insert_mode"):
    c.TerminalInteractiveShell.emacs_bindings_in_vi_insert_mode = False
c.TerminalInteractiveShell.mouse_support = True
