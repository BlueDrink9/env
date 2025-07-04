# Put this line after bindings, otherwise any set bindings will be overridden.
bindkey -v
# Space/leader bindings
leader=" "
# Otherwise backspace doesn't delete previous entries, like vi (not vim).
bindkey -v '^?' backward-delete-char
# Missing for some reason
bindkey -v "^[[3~" delete-char
bindkey -M viins 'kv' vi-cmd-mode
bindkey -M viins 'vk' vi-cmd-mode
bindkey -M viins '^[' vi-cmd-mode

# Make Vi mode transitions faster (KEYTIMEOUT is in hundredths of a second)
# This may be too small for slow remote editing.
export KEYTIMEOUT=20  # Default 40 (400 ms).

# Change cursor shape for different vi modes.
zle-keymap-select () {
  if [[ ${KEYMAP} == vicmd ]] ||
     [[ $1 = 'block' ]]; then
    echo -ne "${term_block_cursor}"

  elif [[ ${KEYMAP} == main ]] ||
       [[ ${KEYMAP} == viins ]] ||
       [[ ${KEYMAP} = '' ]] ||
       [[ $1 = 'beam' ]]; then
    echo -ne "${term_bar_cursor}"
  fi
}
zle -N zle-keymap-select

# Use beam shape cursor for each new prompt.
_fix_cursor() {
   echo -ne "${term_bar_cursor}"
}
precmd_functions+=(_fix_cursor)
# Echoing the cursor change causes powerlevel10k's instant prompt to give a
# huge warning. This silences it.
POWERLEVEL9K_INSTANT_PROMPT=quiet

# Many of these may be better suited to zle widgets, since remapping any of
# the inside keys won't break them?
# Log in to lastpass/add ssh keys
bindkey -sM vicmd "" "ddilastpass_ssh_key_add
"
# Insert the arguments from the last command as if from register @a.
bindkey -sM vicmd "\"ap" "i !!*s" + magic-space
# Jump to the target folder of the last command and run ls with g!
bindkey -sM vicmd "g!" 'cccd !$ && ls\C-m'
# Insert single char in normal mode, like in vim.
bindkey -sM vicmd "," 'i_\er'
# modified vim-style quit.
bindkey -sM vicmd ";q" "ddiexit"
# modified vim-style quit (insert mode)
bindkey -sM viins ";q" "ddiexit"
bindkey -sM viins ";e" "I$EDITOR"

bindkey -sM vicmd "\b" "ddicd ..
"

# Better searching in command mode
bindkey -M vicmd '?' history-incremental-search-backward
bindkey -M vicmd '/' history-incremental-search-forward
bindkey "^S" history-incremental-search-backward
# "n" vi-repeat-search
# "N" vi-rev-repeat-search

# Cycle through history based on characters already typed on the line
autoload -U up-line-or-beginning-search
autoload -U down-line-or-beginning-search
zle -N up-line-or-beginning-search
zle -N down-line-or-beginning-search
# Beginning search with arrow keys. Replaced by plugin, this wasn't working.
bindkey -M vicmd "k" up-line-or-beginning-search
bindkey -M vicmd "j" down-line-or-beginning-search
bindkey '^[[A' up-line-or-beginning-search
bindkey '^[[B' down-line-or-beginning-search
# bindkey "$key[Up]" up-line-or-beginning-search
# bindkey "$key[Down]" down-line-or-beginning-search
# bindkey "$terminfo[kcuu1]" history-substring-search-up
# bindkey "$terminfo[kcud1]" history-substring-search-down
# bindkey -M vicmd "k" up-line-or-history
# bindkey -M vicmd "j" down-line-or-history

bindkey '^?' backward-delete-char
if [ "$TERM" = "xterm-kitty" ]; then
  # ctrl + bs in kitty gives ^h, bs gives ^?.
  bindkey -M viins '^h' backward-kill-word
fi
bindkey -M viins  '^w' backward-kill-word

# allow ctrl-p, ctrl-n for navigate history (standard behaviour)
bindkey '^P' up-history
bindkey '^N' down-history

# `v` is already mapped to visual mode, so we need to use a different key to
# open Vim
autoload edit-command-line; zle -N edit-command-line
quick-edit-command-line(){
  VISUAL="$FCEDIT" edit-command-line
}
zle -N quick-edit-command-line
bindkey -M vicmd "^V" quick-edit-command-line
bindkey -M vicmd "!" quick-edit-command-line

# Expand history automatically on !!
bindkey ' ' magic-space
if [[ "${terminfo[kcbt]}" != "" ]]; then
  bindkey "${terminfo[kcbt]}" reverse-menu-complete   # [Shift-Tab] - move through the completion menu backwards
fi

# Make Ctrl-z also resume background process
fancy-ctrl-z () {
if [[ $#BUFFER -eq 0 ]]; then
  BUFFER="fg"
  zle accept-line
else
  zle push-input
  BUFFER="fg"
  zle accept-line
fi
}
zle -N fancy-ctrl-z
bindkey '^Z' fancy-ctrl-z

if command -v fzf >/dev/null 2>&1; then
  bindkey -M vicmd "${leader}/" fzf-history-widget
  bindkey -M vicmd "${leader}f" fzf-history-widget
  bindkey -M viins '^r' fzf-history-widget
  bindkey -M vicmd "${leader}," fzf-cd-widget
  bindkey -M vicmd "${leader}space" fzf-file-widget
  bindkey -M vicmd "^r" redo
  # Emulates doom M-x
  fuzzy-search-executables-widget () {
    fuzzy-search-executables "$@"
  }
  zle -N fuzzy-search-executables-widget
  bindkey -M vicmd "${leader};" fuzzy-search-executables-widget
fi

if command -v mcfly >/dev/null 2>&1; then
  bindkey -M vicmd "${leader}/" mcfly-history-widget
  bindkey -M viins '^r' mcfly-history-widget
fi

if command -v navi >/dev/null 2>&1; then
  bindkey -M vicmd 'K' _navi_widget
fi

# Vim-surround
autoload -Uz surround
zle -N delete-surround surround
zle -N add-surround surround
zle -N change-surround surround
bindkey -a cs change-surround
bindkey -a ds delete-surround
bindkey -a ys add-surround
bindkey -M visual s add-surround

unset leader
