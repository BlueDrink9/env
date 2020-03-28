ZINIT_DIR="${XDG_DATA_HOME}/zinit/bin"
if [[ -r "${ZINIT_DIR}" ]]; then
  source "${ZINIT_DIR}/zinit.zsh"
else
  echo Zinit dir not found.
  return
fi

# Theme with instant prompt support
zinit ice depth=1; zinit light romkatv/powerlevel10k
[ ! -f "$SCRIPT_DIR/plugins/p10k.zsh" ] || source "$SCRIPT_DIR/plugins/p10k.zsh"
alias plugupdate="zinit self-update; zinit update"
alias plugu="plugupdate"

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
zinit light zdharma/fast-syntax-highlighting

# LS_COLORS for a huge number of filetypes. Some end up hard to see on
# certain themes though.
# For GNU ls (the binaries can be gls, gdircolors, e.g. on OS X when installing the
# coreutils package from Homebrew; you can also use https://github.com/ogham/exa)
# zinit ice atclone"dircolors -b LS_COLORS > c.zsh" atpull'%atclone' pick"c.zsh" nocompile'!'
# zinit light trapd00r/LS_COLORS
# zinit light trapd00r/zsh-syntax-highlighting-filetypes
# ZSH_HIGHLIGHT_HIGHLIGHTERS=(main brackets root)

# Load before syntax highlighting
# Search history for current line.
# zinit ice wait lucid
# zinit light zsh-users/zsh-history-substring-search
# bindkey '^[[A' history-substring-search-up
# bindkey '^[[B' history-substring-search-down
# bindkey -M vicmd 'k' history-substring-search-up
# bindkey -M vicmd 'j' history-substring-search-down

