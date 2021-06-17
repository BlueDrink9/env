import os
import pathlib

# Bindings has to be a startup file, not a config file.
c.InteractiveShellApp.exec_files = [
        str(pathlib.Path(__file__).parent.absolute() / "ipython_bindings.py"),
        ]

## Autoindent IPython code entered interactively.
#  Default: True
# c.InteractiveShell.autoindent = True

## Enable magic commands to be called without the leading %.
#  Default: True
# c.InteractiveShell.automagic = True

## Options for displaying tab completions, 'column', 'multicolumn', and
#  'readlinelike'. These options are for `prompt_toolkit`, see `prompt_toolkit`
#  documentation for more information.
#  Choices: any of ['column', 'multicolumn', 'readlinelike']
#  Default: 'multicolumn'
# c.TerminalInteractiveShell.display_completions = 'multicolumn'

## Shortcut style to use at the prompt. 'vi' or 'emacs'.
c.TerminalInteractiveShell.editing_mode = 'vi'
c.TerminalInteractiveShell.editor = 'myVim'
## Display the current vi mode (when using vi editing mode).
c.TerminalInteractiveShell.prompt_includes_vi_mode = True

## Use 24bit colors instead of 256 colors in prompt highlighting.
if os.getenv("COLORTERM"):
    c.TerminalInteractiveShell.true_color = True
