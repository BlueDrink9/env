# The following lines were added by compinstall
zstyle ':completion:*' auto-description 'arg: %d'
zstyle ':completion:*' completer _expand _complete _ignored _approximate _prefix
zstyle ':completion:*' completions 1
zstyle ':completion:*' expand prefix suffix
zstyle ':completion:*' format 'Completing %d'
zstyle ':completion:*' glob 'NUMERIC > 1'
zstyle ':completion:*' group-name ''
zstyle ':completion:*' insert-unambiguous true
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*' list-prompt %SAt %p: Hit TAB for more, or the character to insert%s
zstyle ':completion:*' matcher-list '' 'm:{[:lower:]}={[:upper:]}' 'm:{[:lower:][:upper:]}={[:upper:][:lower:]} l:|=* r:|=*' 'r:|[._-]=** r:|=**'
zstyle ':completion:*' max-errors 2 numeric
zstyle ':completion:*' menu select=long
zstyle ':completion:*' original true
zstyle ':completion:*' preserve-prefix '//[^/]##/'
zstyle ':completion:*' prompt '`%e'\'''
zstyle ':completion:*' select-prompt %SScrolling active: current selection at %l%s
zstyle :compinstall filename '/home/william/.zshrc'
# End of lines added by compinstall

# cd will never select the parent directory (e.g.: cd ../<TAB>)
zstyle ':completion:*:cd:*' ignore-parents parent pwd

# Completing process IDs with menu selection:
zstyle ':completion:*:*:kill:*' menu yes select
zstyle ':completion:*:kill:*'   force-list always

# Fall into menu selection immediately and to have the words sorted by time:
# zstyle ':completion:*:*:xdvi:*' menu yes select
# zstyle ':completion:*:*:xdvi:*' file-sort time

if [[ $'\e\x5b3D' == "$(echoti cub 3)" ]] &&
     [[ $'\e\x5b33m' == "$(echoti setaf 3)" ]]; then
  zstyle -e ':completion:*' list-colors $'reply=( "=(#b)(${(b)PREFIX})(?)([^ ]#)*=0=0=${PREFIX:+${#PREFIX}D${(l:$#PREFIX:: :):-…}\e\x5b}35=33" )'
fi
zstyle ':completion:*:*(directories|files)*' list-colors ''
