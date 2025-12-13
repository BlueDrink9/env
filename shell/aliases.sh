# vim: foldmethod=marker foldmarker={[},{]}{[}{]}

# ctrl + L often does this anyway though...
alias cl="clear"
alias rl="rlwrap"
alias untar="tar -zxvf"
# $0 to reload shell
alias envupd="git -C \"$DOTFILES_DIR\" pull && git -C \"$DOTFILES_DIR\" push && $0"

if [ -n "$ISTERMUX" ]; then
  alias su="tsu"
fi

case "${OSTYPE}" in
  (*darwin*)
    # OSX specific stuff
    alias setssdir="defaults write com.apple.screencapture location"
    notify(){
      osascript -e 'display notification '"$1"' with title "Custom notification"'
    }
  alias ls="ls -Fh -G"
  alias grep="grep --color=auto"
  # ls and grep should use colours automatically because CLICOLOR is set.
  # Apparently though, don't!

  ;;(*linux*)
  alias ls="ls -Fh --color=auto"
  # enable color support of ls and also add handy aliases
  alias dir='dir --color=auto'
  alias vdir='vdir --color=auto'
  alias grep='grep --color=auto'
  alias fgrep='fgrep --color=auto'
  alias egrep='egrep --color=auto'
  ;;(*) true ;;
esac

# shellcheck disable=SC2139
if command -v eza >/dev/null 2>&1; then
  eza_icons=""
  if [ "$USENF" = 1 ]; then
    eza_icons=" --icons=auto"
  fi
  alias ls="eza""${eza_icons}"
  alias tree="eza --tree""${eza_icons}"
  unset eza_icons
elif command -v lsd >/dev/null 2>&1 && [ "$USENF" = 1 ]; then
  alias ls="lsd"
  alias tree="lsd --tree"
elif command -v exa >/dev/null 2>&1; then
  alias ls="exa"
fi

# ls defined by os-specific stuff above.
alias l='ls -C'
alias lsa="ls -a"
alias ll="ls -al"
alias lt="ls -lart"

alias ..="cd .. && ls"
alias ...="cd ../.."
alias ....="cd ../../.."
alias .....="cd ../../../.."
alias cd..="cd .."

alias m="make"

alias g="git"
alias mg="magit"
alias ga="git add"
alias gca="git commit -a"
alias gc="git commit"
alias gup="git commit --amend --no-edit"
alias gupa="git commit -a --amend --no-edit"
alias gs="git status"
alias gpl="git pull &"
alias gpsh="git push &"
alias dif="git diff --ignore-space-change --color-words"
alias magit="myEmacs --eval '(magit-status)'"

# Custom function with logic for different address formats
alias gcl="git_clone"
# Way nicer and more compact way to view logs. Pass -p to see line differences.
alias glog="git plog"

# Use word-level git diff on any file
alias gdiff="git diff --no-index --ignore-space-change --color-words"

alias rg="rg --smart-case"


editor="myVim"
# Much faster startup for vim without plugins, or ide if I need it.
IDEVim='myVim --cmd "let g:ideMode=1"'
liteVim='myVim --cmd "let g:liteMode=1"'
alias vi="liteVim"
if command -v codium >/dev/null 2>&1; then
  alias code="codium"
fi
# liteVim and nopluginVim have been moved to dedicated scripts, for
# discoverability outside shell.
shelleditor='liteVim +"set ft=sh"'

# You know it, baby. Shouldn't need to use nano ever.
# Should also be getting a nice lite nvim where needed.
export VISUAL="myVim"
export EDITOR="liteVim"
# Calling edit-and-execute-command in readline to open the editor actually
# uses `fc` anyway
export FCEDIT="$shelleditor"
# Don't actually need to set ft (git does this for us),
# but leaving it for specificity.
# giteditor(){ myVim --cmd "let g:liteMode=1" +'set ft=gitcommit' "$@"; }
export GIT_EDITOR='myVim --cmd "let g:liteMode=1" +"set ft=gitcommit"'
# May need to run `sudo update-alternatives --config editor` if this is not
# working.
export SUDO_EDITOR="$(which "$(myVim --print-editor-only)")"

fuzzyEdit(){
  fzf --multi --bind 'enter:become('"$editor"' {+})'
}
# alias ls="ls -CF --color=auto"
# ;e and ;q are also defined, but via readline
alias :q="exit"
alias :e="myVim"
alias e="$editor"
alias ide="$IDEVim"
alias le="$liteVim"
alias lle="nopluginVim"
# For when the system is super super slow.
alias llle="myVim -u NONE -c 'set nocp | inore vk <esc> | inore kv <esc> | nnoremap ; :'"
alias view="liteVim -R"
alias e\?=fuzzyEdit
# alias :Q="exit"
# alias ZZ="exit"
function se(){
  # Ensure sudo command is set (esp useful for vim-eunuch)
  SUDO_COMMAND="sudoedit $@" command sudoedit "$@"
}
alias minivim="vim -u '$DOTFILES_DIR/editors/vim/minirc' -c 'set nocp | inore vk <esc> | inore kv <esc> | nnoremap ; :'"
alias mininvim="nvim -u '$DOTFILES_DIR/editors/vim/minirc' -c 'set nocp | inore vk <esc> | inore kv <esc> | nnoremap ; :'"
openVimSession(){ myVim -c "OpenSession $1"; }
alias os="openVimSession"
alias em="myEmacs"
alias emw="emacs"
alias emc="myEmacsclient"
alias ec='emacsclient --no-wait --create-frame --alternate-editor=""'

# Don't accidentally remove or overwrite files.
# alias cp="cp -i"
# alias mv="mv -i"

# Add an "alert" alias for long running commands.  Use like so:
#   sleep 10; alert
alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'

alias reloadConfig="pkill -USR1"
alias darkColours="base16_onedark"
alias lightColours="base16Reset"
# Preview images in terminal, even over ssh.
alias icat="kitty +kitten icat"

alias dotfe="$editor \"${DOTFILES_DIR}\"/shell/aliases.sh \"${DOTFILES_DIR}\"/shell/functions.sh"
alias stowlocal="stow --dir=\"$HOME/.local/packages\" --target=\"$HOME/.local\""

alias packi="pack install"
alias packr="pack refresh"
alias packu="pack refresh && pack upgrade"
alias packs="pack search"
alias packrm="pack remove"
alias pack\?="pack info"

alias lpssh="lastpass_ssh_key_add"
alias lp="lpass-fzf"
alias mosh="mosh_with_options"
alias ssh="ssh_with_options"
# rg + bat.
alias bgr="batgrep"
z_fzf() {
  [ $# -gt 0 ] && __zoxide_z "$@" && return
  __zoxide_z "$(zoxide query --interactive)"
}
zoxide_init(){
  shell="$(basename "$SHELL")"
  if [ "$shell" = "sh" ]; then
    eval "$(zoxide init posix --hook prompt)"
  else
    eval "$(zoxide init $shell)"
  fi
  z_fzf "$@"
  unalias z 2> /dev/null
  alias z="z_fzf"
}
alias z="zoxide_init"

# use y to change directories with yazi
if command -v yazi >/dev/null 2>&1; then
  function y() {
    local tmp="$(mktemp -t "yazi-cwd.XXXXXX")" cwd
    yazi "$@" --cwd-file="$tmp"
    if cwd="$(command cat -- "$tmp")" && [ -n "$cwd" ] && [ "$cwd" != "$PWD" ]; then
      builtin cd -- "$cwd"
    fi
    rm -f -- "$tmp"
  }
fi

if command -v thefuck >/dev/null 2>&1; then
  # lazy-load thefuck because it's super slow to eval
  if command -v thefuck >/dev/null 2>&1; then
    fuck() {
      eval -- "$(thefuck -a)"
      fuck "$@"
    }
  fi
fi
if command -v pay-respects >/dev/null 2>&1; then
  shell="$(basename "$SHELL")"
  eval "$(pay-respects "$shell" --alias)"
fi


# For AUR packages
alias srcinfo='makepkg --printsrcinfo > .SRCINFO'

# Allows pip to auto-update, and ensures you use the right version of pip for the current environment
alias py='python'
alias pip='python -m pip'
alias venv='python -m venv'
alias venvc='python -m venv venv'
alias venvcs='python -m venv venv --system-site-packages'
alias venva='. venv/bin/activate || . .venv/bin/activate || . venv/Scripts/activate || .venv/Scripts/activate'
alias pym='python -m'
alias ipython='python -m IPython'
alias fix-nix-python='nix shell github:GuillaumeDesforges/fix-python -c fix-python --venv venv'

alias plasma_reload='qdbus org.kde.plasmashell /PlasmaShell org.kde.PlasmaShell.refreshCurrentShell'
alias lsblkids='lsblk -o NAME,SIZE,MOUNTPOINT,UUID,PARTUUID'
alias goRouter='fopen $(ip route | cut -f3 -d" " | head -n 1)'
alias winkill='kill $(xprop | rg pid | cut -d" " -f3)'
alias snatch='git clone --depth=1'
alias git-update-all='for f in *; do git -C $f pull; done'
alias syncthing-remote='"ssh" -L 8385:localhost:8384'

function rgedit() {
    myVim $(rg --files-with-matches "$@")
}
alias rge="rgedit"

alias bm="cd \"\$(bmm)\""
# Delete to trash
alias del="gio trash"
alias clip='xclip -selection clipboard'

flakegit=""
if [ -d /etc/nixos/.git ]; then
  flakegit="--commit-lock-file"
fi
alias nixup="sudo nix flake update /etc/nixos/ $flakegit"
unset flakegit
nom=""
if command -v nom >/dev/null 2>&1; then
  nom="--log-format internal-json -v |& nom --json"
fi
alias renix="sudo -v && sudo DOTFILES_DIR="$DOTFILES_DIR" nixos-rebuild switch --impure $nom"
alias homer="(command -v home-manager > /dev/null && home-manager switch $nom) || nix-shell '<home-manager>' -A install"
alias homeu="nix-channel --update"
unset nom
git_fake_add() {
  git add --intent-to-add "$@"
  git update-index --skip-worktree --assume-unchanged "$@"
}
nix_cache_query(){
  drv="$1"
  # Strip to after last /
  drv="${1##*/}"
  # Strip to before -
  drv="${1%%-*}"
  curl "https://cache.nixos.org/${drv}.narinfo"
}

alias nix-shell='nix-shell --run $(basename $SHELL)'
alias ns='nix-shell'
alias np='nix-shell -p'
alias npu='nix-shell -I nixpkgs=channel:nixos-unstable -p'
alias sysmanr='sudo "$(which nix)" run --impure "github:numtide/system-manager" -- switch --flake $DOTFILES_DIR/nix/system-manager --nix-option pure-eval false'

alias dva="direnv allow"
function nsc(){
  nix flake init -t "github:the-nix-way/dev-templates#$1"
  echo "use flake" >> .envrc
  direnv allow
}
if command -v radian >/dev/null 2>&1; then
  alias r="radian"
fi
alias c="aichat"

alias talon-update-plugins="$DOTFILES_DIR/talon/sync_plugins.sh"
