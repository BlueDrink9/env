# vim: foldmethod=marker foldmarker={[},{]}{[}{]}

# ctrl + L often does this anyway though...
alias cl="clear"
alias rl="rlwrap"
alias untar="tar -zxvf"
alias envupd="git -C \"$DOTFILES_DIR\" pull && $0"

if [ -n "$ISTERMUX" ]; then
  alias su="tsu"
fi

if [[ "$OSTYPE" =~ "darwin" ]]; then  # OSX specific stuff
  alias setssdir="defaults write com.apple.screencapture location"
  notify(){
    osascript -e 'display notification '"$1"' with title "Custom notification"'
  }
  alias ls="ls -Fh -G"
  alias grep="grep --color=auto"
  alias sudoedit="sudo -e"
  # ls and grep should use colours automatically because CLICOLOR is set.
  # Apparently though, don't!

elif [[ "$OSTYPE" =~ "linux" ]]; then  # Linux specific stuff
  alias ls="ls -Fh --color=auto"
  # enable color support of ls and also add handy aliases
  alias dir='dir --color=auto'
  alias vdir='vdir --color=auto'
  alias grep='grep --color=auto'
  alias fgrep='fgrep --color=auto'
  alias egrep='egrep --color=auto'
fi

if command -v lsd >/dev/null 2>&1 && [ "$USENF" = 1 ]; then
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

alias g="git"
alias ga="git add"
alias gca="git commit -a"
alias gc="git commit"
alias gup="git commit --amend --no-edit"
alias gupa="git commit -a --amend --no-edit"
alias gs="git status"
alias gpl="git pull &"
alias gpsh="git push &"
alias dif="git diff --ignore-space-change --color-words"
alias magit="myEmacs --no-window-system --eval='(magit-status)'"

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
alias vi="liteVim"
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
export SUDO_EDITOR=vim

fuzzyEdit(){
  $editor "$(fzf)"
}
# alias ls="ls -CF --color=auto"
# ;e and ;q are also defined, but via readline
alias :q="exit"
alias :e="myVim"
alias e="$editor"
alias ide="$IDEVim"
alias le="$iteVim"
alias lle="nopluginVim"
# For when the system is super super slow.
alias llle="myVim -u NONE -c 'set nocp | inore vk <esc> | inore kv <esc> | nnoremap ; :'"
alias view="liteVim -R"
alias e\?=fuzzyEdit
# alias :Q="exit"
# alias ZZ="exit"
alias se="sudoedit"
alias minivim="vim -u '$DOTFILES_DIR/editors/vim/minirc' -c 'set nocp | inore vk <esc> | inore kv <esc> | nnoremap ; :'"
alias mininvim="nvim -u '$DOTFILES_DIR/editors/vim/minirc' -c 'set nocp | inore vk <esc> | inore kv <esc> | nnoremap ; :'"
openVimSession(){ myVim -c "OpenSession $1"; }
alias os="openVimSession"

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
alias mosh="mosh_with_options"
alias ssh="ssh_with_options"
if command -v batgrep >/dev/null 2>&1; then
  # rg + bat.
  alias bg="batgrep"
fi

if command -v thefuck >/dev/null 2>&1; then
  eval $(thefuck --alias)
fi

# For AUR packages
alias srcinfo='makepkg --printsrcinfo > .SRCINFO'
