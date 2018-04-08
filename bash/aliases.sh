# -*-  mode: shell-script; -*-
# vim: set ft=sh:

# Always used
shopt -s expand_aliases

# ctrl + L often does this anyway though...
alias cl="clear"

alias ..="cd .. && ls"
alias cd..="cd .. && ls"
alias l='ls -CF'
alias lsa="ls -aF"
alias ll="ls -alF"
alias lss="ls -lsh"
alias ls="ls -CF"
# TODO make this more flexible (method?)
alias open="nautilus $1 >/dev/null 2>&1"
# alias :q="exit"
# alias :Q="exit"
# alias ZZ="exit"

alias g="git"
alias gca="git commit -a"
alias gco="git commit"
alias gs="git status"
# Custom function with logic for different address formats
alias gc="git_clone"

# Try to set vim to xvim (has x11 clipboard support)
if [ -e /usr/bin/vimx ]; then
    alias vim='/usr/bin/vimx'
elif [ command -v mvim 2>/dev/null ]; then
    # Prefer macvim to gvim.
    alias vim='mvim -v'
elif [ command -v gvim 2>/dev/null ]; then
    alias vim='gvim -v'
elif [ -e /usr/bin/gvim ]; then
    alias vim='/usr/bin/gvim -v'
fi
# Much faster startup for vim without plugins.
alias qvim="vim --noplugin"
alias vi="vim --noplugin"

# Prevent files from being overwritten by redirection.
set -o noclobber
# Don't accidentally remove or overwrite files.
# alias cp="cp -i"
# alias mv="mv -i"


alias sagi="sudo apt install"
alias sag="sudo apt"
alias sagu="sudo apt update && sudo apt upgrade"

# enable color support of ls and also add handy aliases
# if [ -x /usr/bin/dircolors ]; then
#     test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
#     alias ls='ls --color=auto'
#     alias dir='dir --color=auto'
#     alias vdir='vdir --color=auto'
#
#     alias grep='grep --color=auto'
#     alias fgrep='fgrep --color=auto'
#     alias egrep='egrep --color=auto'
# fi
