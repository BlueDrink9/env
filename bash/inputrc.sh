# vim: set ft=sh:
# Readline settings

include /etc/inputrc
# Set bash movement keys to be more vi-like
# Can use `set -o vi` for short version
set editing-mode vi

# Used for set -o, shows a symbol at start of prompt for bash vi mode
set show-mode-in-prompt on
# Doesn't seem to expand env variables.
# Readline prompt-escape is \1\2 instead of \[\].
# Yellow for ins, green for norm. Same as vim CS.
set vi-ins-mode-string \1\e[0;33m\2++\1\e[m\2
set vi-cmd-mode-string \1\e[0;32m\2::\1\e[m\2

# Completions with no shared prefix will be listed.
set show-all-if-unmodified on
# Ignore case
set completion-ignore-case on
# Only show next few chars of name when completing, so you can see what you
# need to type.
set completion-prefix-display-length 4
# Hyphens and underscores are treated the same by completion.
set completion-map-case on
# Show all completions as soon as I press tab, even if there's more than one
set show-all-if-ambiguous on
# on menu-complete, first display the common prefix, then cycle through the
# options when hitting TAB
set menu-complete-display-prefix on

# Colour common prefixes of tab-completion
set colored-completion-prefix on
# Colour completion based on file type (like ls)
set colored-stats on
# Symlinks get completed with trailing /
set mark-symlinked-directories on
# Symbol after file specifying type
set visible-stats on
# Autocomplete hidden files without needing dot.
set match-hidden-files on
# If there are more than 40 possible completions for a word, ask to show them all
set completion-query-items 70
# Be more intelligent when autocompleting by also looking at the text after
# the cursor. For example, when the current line is "cd ~/src/mozil", and
# the cursor is on the "z", pressing Tab will not autocomplete it to "cd
# ~/src/mozillail", but to "cd ~/src/mozilla". (This is supported by the
# Readline used by Bash 4.)
set skip-completed-text on

# Expand history automatically on !!
$if Bash
  Space: magic-space
$endif

# Neat feature that detects pasted chars without running them.
set enable-bracketed-paste on

# Disable bell on error
set bell-style none

###########################################################
# Keymaps for when we're in command mode (e.g., after hitting ESC)
set keymap vi-command

# Insert the arguments from the last command as if from register @a.
"\"ap": "i !!*\r"

# When hitting up/down, cycle through the previous commands
# which start with the prefix you've entered, rather than just cycling through
# the last entered commands.
"\e[A": history-search-backward
"\e[B": history-search-forward
"j":history-search-forward
"k":history-search-backward

# modified vim-style quit.
# ";q":"ccexit\"
";q":"ccexit"

###########################################################
# Keymaps for when we're in insert (i.e., typing stuff in) mode
set keymap vi-insert

#"kv" will enter vi command mode. Use ctrl+v to avoid.
"kv":vi-movement-mode
"vk":vi-movement-mode

# modified vim-style quit.
# ";q":"ccexit\"
";q":"ccexit"

# Pressing tab will list all completions & select the first one. Pressing it
# again will cycle through available completions.
# Commented because undesirable (can't type letter to jump to selection.)
Tab: menu-complete
# Shift-TAB cycles completions backward
"\e[Z": menu-complete-backward
# "\C-n": menu-complete
# "\C-p": menu-complete-backward

# up/down should also apply to insert mode
"\e[A": history-search-backward
"\e[B": history-search-forward

# Needed because binding 'p' in command mode above wipes its insert mode
# function, too. This fixes that, and will insert 'p' when you type 'p'.
"P": self-insert

# ctrl + backspace deletes word
# Doesn't work on all systems, needs looking at.
# "\C-?":"\C-W"
