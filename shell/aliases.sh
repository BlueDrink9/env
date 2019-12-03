# vim: foldmethod=marker foldmarker={[},{]}{[}{]}

# Always used
shopt -s expand_aliases

# ctrl + L often does this anyway though...
alias cl="clear"
alias rl="rlwrap"
alias untar="tar -zxvf"
alias envupd="git -C \"$DOTFILES_DIR\" pull && . ~/.bashrc"

if [[ "$OSTYPE" =~ "darwin" ]]; then  # OSX specific stuff
  alias setssdir="defaults write com.apple.screencapture location"
  notify(){
    osascript -e 'display notification '"$1"' with title "Custom notification"'
  }
  alias ls="ls -Fh -G"
  alias grep="grep --color=auto"
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
if [ -n "$ISTERMUX" ]; then
  alias sudo="tsudo"
  alias su="tsu"
fi

# ls defined by os-specific stuff above.
alias l='ls -C'
alias lsa="ls -a"
alias ll="ls -al"
alias lt="ls -lhart"
alias lss="ls -ls"

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
alias dif="git diffw"
# Custom function with logic for different address formats
alias gc="git_clone"
# Way nicer and more compact way to view logs. Pass -p to see line differences.
alias glog="git log --color --graph --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset' --abbrev-commit"

# Try to set vim to better versions.
# Will be expanded by functions, but not variables.
if [ $(command -v nvim 2>/dev/null) ]; then
  alias myVim="nvim"
elif [ $(command -v mvim 2>/dev/null) ]; then
  # Prefer macvim to gvim.
  alias myVim="mvim -v"
elif [ $(command -v gvim 2>/dev/null) ]; then
  alias myVim="gvim -v"
elif [ -e /usr/bin/gvim ]; then
  alias myVim="/usr/bin/gvim -v"
  # xvim (has x11 clipboard support)
elif [ -e /usr/bin/vimx ]; then
  alias myVim="/usr/bin/vimx"
else
  alias myVim="vim"
  unset -f vim
fi

vimAlias="$(alias myVim)"
# Extract between quotes.
vimTmp="${vimAlias#*\'}"
MYVIM="${vimTmp%\'*}"
unset vimAlias vimTmp

# Prevent recursive 'vim' calls.
if [ ! "${MYVIM}" = "vim" ]; then
  # Aliases aren't expanded in variables, eg $SUDO_EDITOR
  vim(){
    myVim "$@"
  }
fi

# Much faster startup for vim without plugins, or ide if I need it.
IDEVim(){ vim --cmd "let g:ideMode=1" "$@"; }
alias idevim="IDEVim"
liteVim(){ vim --cmd "let g:liteMode=1" "$@"; }
alias vi="liteVim"
alias view="vi -R"
nopluginVim(){ vim --noplugin --cmd "let g:noPlugins=1" "$@"; }
alias lvi="nopluginVim"
shelleditor(){ vim --cmd "let g:liteMode=1" +'set ft=sh' "$@"; }

# You know it, baby. Shouldn't need to use nano ever.
# Should also be getting a nice lite nvim where needed.
export VISUAL=liteVim
export EDITOR=nopluginVim
# export EDITOR=liteVim
# Calling edit-and-execute-command in readline to open the editor actually
# uses `fc` anyway
export FCEDIT=shelleditor
giteditor(){ vim --cmd "let g:liteMode=1" +'set ft=gitcommit'; }
# The function trick doesn't work with git, but regular arguments do. Means
# we don't get the best vim version from the alias though.
# Don't actually need to set ft (git does this for us),
# but leaving it for specificity.
export GIT_EDITOR_CMD="${MYVIM}"' --cmd "let g:liteMode=1" +"set ft=gitcommit"'
# export GIT_EDITOR_CMD=$(type liteVim | head -n4 | tail -n1)
export GIT_EDITOR="$GIT_EDITOR_CMD"
# May need to run `sudo update-alternatives --config editor` if this is not
# working.
export SUDO_EDITOR=vim

fuzzyEdit(){
  "$VISUAL" "$(fzf)"
}
# alias ls="ls -CF --color=auto"
# ;e and ;q are also defined, but via readline
alias :q="exit"
alias :e="vim"
alias e="vim"
alias ide="IDEVim"
alias e?=fuzzyEdit
# For when the system is so slow it makes you say "what the vuck?"
alias wtv="vim -u NONE -c 'set nocp | inore vk <esc> | inore kv <esc> | nnoremap ; :'"
# alias :Q="exit"
# alias ZZ="exit"
alias se="sudoedit"

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

alias dotfe="vim \"${DOTFILES_DIR}\"/shell/aliases.sh \"${DOTFILES_DIR}\"/shell/functions.sh"
alias stowlocal="stow --dir=\"$HOME/.local/packages\" --target=\"$HOME/.local\""




# https://superuser.com/a/437508/885475
# Automatically add completion for all aliases to commands having completion functions
function alias_completion {
    local namespace="alias_completion"

    # parse function based completion definitions, where capture group 2 => function and 3 => trigger
    local compl_regex='complete( +[^ ]+)* -F ([^ ]+) ("[^"]+"|[^ ]+)'
    # parse alias definitions, where capture group 1 => trigger, 2 => command, 3 => command arguments
    local alias_regex="alias ([^=]+)='(\"[^\"]+\"|[^ ]+)(( +[^ ]+)*)'"

    # create array of function completion triggers, keeping multi-word triggers together
    eval "local completions=($(complete -p | sed -Ene "/$compl_regex/s//'\3'/p"))"
    (( ${#completions[@]} == 0 )) && return 0

    # create temporary file for wrapper functions and completions
    rm -f "/tmp/${namespace}-*.tmp" # preliminary cleanup
    local tmp_file; tmp_file="$(mktemp "/tmp/${namespace}-${RANDOM}XXX.tmp")" || return 1

    local completion_loader; completion_loader="$(complete -p -D 2>/dev/null | sed -Ene 's/.* -F ([^ ]*).*/\1/p')"

    # read in "<alias> '<aliased command>' '<command args>'" lines from defined aliases
    local line; while read line; do
        eval "local alias_tokens; alias_tokens=($line)" 2>/dev/null || continue # some alias arg patterns cause an eval parse error
        local alias_name="${alias_tokens[0]}" alias_cmd="${alias_tokens[1]}" alias_args="${alias_tokens[2]# }"

        # skip aliases to pipes, boolean control structures and other command lists
        # (leveraging that eval errs out if $alias_args contains unquoted shell metacharacters)
        eval "local alias_arg_words; alias_arg_words=($alias_args)" 2>/dev/null || continue
        # avoid expanding wildcards
        read -a alias_arg_words <<< "$alias_args"

        # skip alias if there is no completion function triggered by the aliased command
        if [[ ! " ${completions[*]} " =~ " $alias_cmd " ]]; then
            if [[ -n "$completion_loader" ]]; then
                # force loading of completions for the aliased command
                eval "$completion_loader $alias_cmd"
                # 124 means completion loader was successful
                [[ $? -eq 124 ]] || continue
                completions+=($alias_cmd)
            else
                continue
            fi
        fi
        local new_completion="$(complete -p "$alias_cmd")"

        # create a wrapper inserting the alias arguments if any
        if [[ -n $alias_args ]]; then
            local compl_func="${new_completion/#* -F /}"; compl_func="${compl_func%% *}"
            # avoid recursive call loops by ignoring our own functions
            if [[ "${compl_func#_$namespace::}" == $compl_func ]]; then
                local compl_wrapper="_${namespace}::${alias_name}"
                    echo "function $compl_wrapper {
                        (( COMP_CWORD += ${#alias_arg_words[@]} ))
                        COMP_WORDS=($alias_cmd $alias_args \${COMP_WORDS[@]:1})
                        (( COMP_POINT -= \${#COMP_LINE} ))
                        COMP_LINE=\${COMP_LINE/$alias_name/$alias_cmd $alias_args}
                        (( COMP_POINT += \${#COMP_LINE} ))
                        $compl_func
                    }" >> "$tmp_file"
                    new_completion="${new_completion/ -F $compl_func / -F $compl_wrapper }"
            fi
        fi

        # replace completion trigger by alias
        new_completion="${new_completion% *} $alias_name"
        echo "$new_completion" >> "$tmp_file"
    done < <(alias -p | sed -Ene "s/$alias_regex/\1 '\2' '\3'/p")
    source "$tmp_file" && rm -f "$tmp_file"
  }; alias_completion
