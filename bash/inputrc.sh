# vim: set ft=sh:
# Readline settings

include /etc/inputrc
# Set bash movement keys to be more vi-like
# Can use `set -o vi` for short version
set editing-mode vi

# Used for set -o, shows a symbol at start of prompt for bash vi mode
set show-mode-in-prompt on

# Show all completions as soon as I press tab, even if there's more than one.
# On menu-complete, first display the common prefix, then cycle through the
# options when hitting TAB
set menu-complete-display-prefix on
set show-all-if-ambiguous on
set show-all-if-unmodified on
# Ignore case
set completion-ignore-case on

# Colour common prefixes of tab-completion
set colored-completion-prefix on
# Colour completion based on file type (like ls)
set colored-stats on
# Case insensitive completion
# set completion-ignore-case on
# Symlinks get completed with trailing /
set mark-symlinked-directories on
# Symbol after file specifying type
set visible-stats on

# Neat feature that detects pasted chars without running them.
set enable-bracketed-paste on

# Disable bell on error
set bell-style none

###########################################################
# Keymaps for when we're in command mode (e.g., after hitting ESC)
set keymap vi-command

# Insert the arguments from the last command with p. Use P to paste
"p": "i !!*\r"

# When hitting up/down, cycle through the previous commands
# which start with the prefix you've entered, rather than just cycling through
# the last entered commands.
"\e[A": history-search-backward
"\e[B": history-search-forward
j:history-search-forward
k:history-search-backward

###########################################################
# Keymaps for when we're in insert (i.e., typing stuff in) mode
set keymap vi-insert


# Pressing tab will list all completions & select the first one. Pressing it 
# again will cycle through available completions.
# Commented because undesirable (can't type letter to jump to selection.)
Tab: menu-complete
# Shift-TAB cycles completions backward
\e[Z: menu-complete-backward
# "\C-n": menu-complete
# "\C-p": menu-complete-backward

# up/down should also apply to insert mode
"\e[A": history-search-backward
"\e[B": history-search-forward

# Needed because binding 'p' in command mode above wipes its insert mode
# function, too. This fixes that, and will insert 'p' when you type 'p'.
"p": self-insert
"i": self-insert
