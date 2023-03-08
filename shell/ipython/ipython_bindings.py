from IPython import get_ipython
from prompt_toolkit.enums import DEFAULT_BUFFER
from prompt_toolkit.filters import HasFocus, ViInsertMode, ViNavigationMode
from prompt_toolkit.key_binding.vi_state import InputMode
import IPython.terminal.shortcuts as shortcuts
from prompt_toolkit.keys import Keys


ip = get_ipython()

def switch_to_navigation_mode(event):
   vi_state = event.cli.vi_state
   vi_state.input_mode = InputMode.NAVIGATION


# Register the shortcut if IPython is using prompt_toolkit
if getattr(ip, 'pt_app', None):
    registry = ip.pt_app.key_bindings
elif (getattr(ip, 'pt_cli', None)):
    # for IPython versions 5.x
    registry = ip.pt_cli.application.key_bindings_registry

if registry:
    registry.add_binding('k', 'v',
                         filter=(HasFocus(DEFAULT_BUFFER)
                                 & ViInsertMode()))(switch_to_navigation_mode)
    registry.add_binding('v', 'k',
                         filter=(HasFocus(DEFAULT_BUFFER)
                                 & ViInsertMode()))(switch_to_navigation_mode)

    # c.TerminalInteractiveShell.shortcuts.extend([
        
    # ])

    registry.add_binding('g', Keys.ControlV,
                         filter=(HasFocus(DEFAULT_BUFFER)
                                 & ViNavigationMode()))(
                                     shortcuts.open_input_in_editor)



