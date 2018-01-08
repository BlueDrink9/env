# Removes carriage return characters.
function rmcr() {
    sed -i 's/\r$//' $1
}

# Replaces a file with the .bak version of itself.
function mkbak() {
    cp $1 $1.bak
}

function mkcd() {
    mkdir -p $1 && cd $1
}

alias cl="clear"
alias ..="cd .. && ls"
alias cd..="cd .. && ls"
alias lsa="ls -al"
alias gc="git clone"
alias open="nautilus $1 >/dev/null 2>&1"
alias :q='exit'

if [ -d "$HOME/.redis" ]; then
    alias redis="cd $HOME/.redis/redis-server"
fi

if [ -d "$HOME/workspace" ]; then
    alias ws="cd $HOME/workspace"
fi
