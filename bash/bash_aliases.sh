# Removes carriage return characters.
function rmcr() { 
    sed -i 's/\r$//' $1
}

# Replaces a file with the .bak version of itself.
function mkbak() { 
    cp $1 $1.bak
}

alias cl="clear"
alias ..="cd .."
alias cd..="cd .."
alias lsa="ls -al"
alias gc="git clone"

if [ -d "$HOME/workspace" ]; then
    alias ws="cd $HOME/workspace"
fi
