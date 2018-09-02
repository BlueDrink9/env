# -*-  mode: shell-script; -*-
# vim: set ft=sh:

# Always used
shopt -s expand_aliases

# ctrl + L often does this anyway though...
alias cl="clear"

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
# alias ls="ls -CF --color=auto"
alias :q="exit"
alias :e="vim"
alias e="vim"
# alias :Q="exit"
# alias ZZ="exit"

alias g="git"
alias gca="git commit -a"
alias gco="git commit"
alias gup="git commit --amend --no-edit"
alias gupa="git commit -a --amend --no-edit"
alias gs="git status"
alias dif="git diff"
# Custom function with logic for different address formats
alias gc="git_clone"
# Way nicer and more compact way to view logs. Pass -p to see line differences.
alias glog="git log --color --graph --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset' --abbrev-commit"

# Try to set vim to xvim (has x11 clipboard support)
if [ -e /usr/bin/vimx ]; then
    alias vim="/usr/bin/vimx"
elif [ `command -v mvim 2>/dev/null` ]; then
    # Prefer macvim to gvim.
    alias vim="mvim -v"
elif [ `command -v gvim 2>/dev/null` ]; then
    alias vim="gvim -v"
elif [ -e /usr/bin/gvim ]; then
    alias vim="/usr/bin/gvim -v"
fi

alias idevim="vim --cmd \"let g:ideMode=1\""
# Much faster startup for vim without plugins.
alias vi="vim --cmd \"let g:liteMode=1\""
alias lvi="vim --noplugin --cmd \"let g:noPlugins=1\""

# Prevent files from being overwritten by redirection.
set -o noclobber
# Don't accidentally remove or overwrite files.
# alias cp="cp -i"
# alias mv="mv -i"


alias svi="sudoedit"
alias sagi="sudo apt install"
alias sag="sudo apt"
alias sagu="sudo apt update && sudo apt upgrade"

# Add an "alert" alias for long running commands.  Use like so:
#   sleep 10; alert
alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'
