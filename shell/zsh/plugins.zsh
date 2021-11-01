ZINIT_DIR="${XDG_DATA_HOME}/zinit/bin"
if [[ -r "${ZINIT_DIR}" ]]; then
  source "${ZINIT_DIR}/zinit.zsh"
else
  echo Zinit dir not found.
  return
fi

alias plugupdate="zinit self-update; zinit update"
alias plugu="plugupdate"

# Theme with instant prompt support
zinit ice depth=1; zinit light romkatv/powerlevel10k
[ ! -f "$SCRIPT_DIR/plugins/p10k.zsh" ] || source "$SCRIPT_DIR/plugins/p10k.zsh"

# Prevents kitty set-colours from swallowing input
precmd_functions=("${(@)precmd_functions:#_prompt_command}")
zinit ice depth=1; zinit light romkatv/zsh-defer
# Restore the precmd_function
zsh-defer -a -c "precmd_functions+=(_prompt_command)"
zsh-defer -a -c "$PROMPT_COMMAND"

# Delay load, skip print when loaded.
zinit ice wait lucid atload'_zsh_autosuggest_start'
zinit light zsh-users/zsh-autosuggestions
zle -N autosuggest-accept  # Dummy binding to supress error. Replaced later.
bindkey -M viins "" autosuggest-accept
ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE="fg=014"
# Completion requires zpty
ZSH_AUTOSUGGEST_STRATEGY=(history completion)
ZSH_AUTOSUGGEST_BUFFER_MAX_SIZE=40
ZSH_AUTOSUGGEST_USE_ASYNC=1

zinit ice wait lucid
zinit light wazum/zsh-directory-dot-expansion


# Binary release in archive, from GitHub-releases page.
# After automatic unpacking it provides program "fzf".
zinit ice wait lucid from"gh-r" as"program"
zinit load junegunn/fzf-bin

zinit ice wait lucid
zinit light liangguohuan/zsh-dircolors-solarized
export DIRCOLORTHEME='dircolors.ansi-universal'

# zinit ice wait lucid
zinit light zdharma-continuum/fast-syntax-highlighting

# This speeds up pasting w/ autosuggest
# https://github.com/zsh-users/zsh-autosuggestions/issues/238
autoload -U url-quote-magic
pasteinit() {
  OLD_SELF_INSERT=${${(s.:.)widgets[self-insert]}[2,3]}
  zle -N self-insert url-quote-magic
}
pastefinish() {
  zle -N self-insert $OLD_SELF_INSERT
}
zstyle :bracketed-paste-magic paste-init pasteinit
zstyle :bracketed-paste-magic paste-finish pastefinish

# LS_COLORS for a huge number of filetypes. Some end up hard to see on
# certain themes though.
# For GNU ls (the binaries can be gls, gdircolors, e.g. on OS X when installing the
# coreutils package from Homebrew; you can also use https://github.com/ogham/exa)
# zinit ice atclone"dircolors -b LS_COLORS > c.zsh" atpull'%atclone' pick"c.zsh" nocompile'!'
# zinit light trapd00r/LS_COLORS
# zinit light trapd00r/zsh-syntax-highlighting-filetypes
# ZSH_HIGHLIGHT_HIGHLIGHTERS=(main brackets root)

# zinit light Aloxaf/fzf-tab
# # disable sort when completing `git checkout`
# zstyle ':completion:*:git-checkout:*' sort false
# # set descriptions format to enable group support
# zstyle ':completion:*:descriptions' format '[%d]'
# # set list-colors to enable filename colorizing
# zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
# # preview directory's content with exa when completing cd
# zstyle ':fzf-tab:complete:cd:*' fzf-preview 'ls -1 --color=always $realpath'
# # switch group using `,` and `.`
# zstyle ':fzf-tab:*' switch-group ',' '.'

# Load before syntax highlighting
# Search history for current line.
# zinit ice wait lucid
# zinit light zsh-users/zsh-history-substring-search
# bindkey '^[[A' history-substring-search-up
# bindkey '^[[B' history-substring-search-down
# bindkey -M vicmd 'k' history-substring-search-up
# bindkey -M vicmd 'j' history-substring-search-down

