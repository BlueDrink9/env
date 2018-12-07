# vim: set ft=sh:
# vim: set tw=78 ts=4 sw=4
# vim: foldmethod=marker foldmarker={[},{]}

# Always used
shopt -s expand_aliases

# ctrl + L often does this anyway though...
alias cl="clear"
alias rl="rlwrap"
alias untar="tar -zxvf"
alias envupd="git -C \"$DOTFILES_DIR\" pull && . ~/.bashrc"

if [[ "$OSTYPE" =~ "darwin1" ]]; then  # OSX specific stuff
    alias setssdir="defaults write com.apple.screencapture location"
    alias ls="ls -Fh -G"
    # ls and grep should use colours automatically because CLICOLOR is set.
    # Make esc act as backspace in terminal

elif [ "$OSTYPE" = "linux-gnu" ]; then  # Linux specific stuff
    alias ls="ls -Fh --color=auto"
    # enable color support of ls and also add handy aliases
    alias dir='dir --color=auto'
    alias vdir='vdir --color=auto'
    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

# ls defined by os-specific stuff above.
alias l='ls -C'
alias lsa="ls -a"
alias ll="ls -al"
alias lss="ls -ls"

# Keyboard setup stuff
capsToBS="-option caps:backspace"
altWinSwap="-option altwin:swap_alt_win"
altShiftToggle="-option grp:alt_shift_toggle"
capsLed="-option grp_led:caps"
colemak="-layout 'us, us' -variant 'colemak,'"
alias wwkb="setxkbmap $colemak $capsToBS $altWinSwap $altShiftToggle $capsLed"

alias ..="cd .. && ls"
alias ...="cd ../.."
alias ....="cd ../../.."
alias .....="cd ../../../.."
alias cd..="cd .."

alias g="git"
alias gca="git commit -a"
alias gco="git commit"
alias gup="git commit --amend --no-edit"
alias gupa="git commit -a --amend --no-edit"
alias gs="git status"
alias gpl="git pull &"
alias gpsh="git push &"
alias dif="git diff"
# Custom function with logic for different address formats
alias gc="git_clone"
# Way nicer and more compact way to view logs. Pass -p to see line differences.
alias glog="git log --color --graph --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset' --abbrev-commit"

# Try to set vim to better versions...
if [ `command -v nvim 2>/dev/null` ]; then
    alias vim="nvim"
elif [ `command -v mvim 2>/dev/null` ]; then
    # Prefer macvim to gvim.
    alias vim="mvim -v"
elif [ `command -v gvim 2>/dev/null` ]; then
    alias vim="gvim -v"
elif [ -e /usr/bin/gvim ]; then
    alias vim="/usr/bin/gvim -v"
# xvim (has x11 clipboard support)
elif [ -e /usr/bin/vimx ]; then
    alias vim="/usr/bin/vimx"
fi

alias idevim="vim --cmd \"let g:ideMode=1\""
# Much faster startup for vim without plugins.
alias vi="vim --cmd \"let g:liteMode=1\""
alias lvi="vim --noplugin --cmd \"let g:noPlugins=1\""
alias view="vi -R"

# alias ls="ls -CF --color=auto"
alias :q="exit"
alias :e="vim"
alias e="vim"
# alias :Q="exit"
# alias ZZ="exit"
alias svi="sudoedit"

# Don't accidentally remove or overwrite files.
# alias cp="cp -i"
# alias mv="mv -i"

# {[} Package manager
installcmd="install"
refreshcmd="update"
upgradecmd="upgrade"
searchcmd="search"
removecmd="remove"
if [ `command -v brew 2>/dev/null` ]; then
    alias pack="brew"
    removecmd="uninstall"
elif [ `command -v pacman 2>/dev/null` ]; then
    if [ `command -v yay 2>/dev/null` ]; then
        alias pack="yay"
    else
        alias pack="pacman"
    fi
    installcmd="-S"
    refreshcmd="-S"
    upgradecmd="-U"
    searchcmd="-Ss"
    removecmd="-R"
elif [ `command -v pkg 2>/dev/null` ]; then
    # Probably termux, may be freeBSD.
    alias pack="pkg"
elif [ `command -v apt 2>/dev/null` ]; then
    alias pack="sudo apt"
elif [ `command -v yum 2>/dev/null` ]; then
    alias pack="sudo yum"
fi
alias packi="pack $installcmd"
alias packr="pack $refreshcmd"
alias packu="pack $refreshcmd && pack $upgradecmd"
alias packs="pack $searchcmd"
alias packrm="pack $removecmd"
unset installcmd refreshcmd upgradecmd searchcmd removecmd

# {]} Package manager

# Add an "alert" alias for long running commands.  Use like so:
#   sleep 10; alert
alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'
