# Removes carriage return characters.
function noCR { 
    sed -i 's/\r$//' $1
}

# Replaces a file with the .bak version of itself.
function bak { 
    mv $1 $1.bak
}

alias cl='clear'
alias ..='cd ..'
alias cd..='cd ..'
alias lsa='ls -al'
alias gc='git clone'

