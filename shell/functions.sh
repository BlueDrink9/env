# vim:ts=2:sw=2
# vim:foldmarker={[},{]}:foldmethod=marker
# This file holds reusable functions
[ -n "${SHELL_FUNCTIONS_LOADED}" ] && return || SHELL_FUNCTIONS_LOADED=1

# Checks if the first arg is a substring of the second.
substrInStr() {
  substring="$1"
  string="$2"
  case "${string?No string given}" in
    (*${substring?No substring given}*)  return 0 ;;
    (*)                          return 1 ;;
  esac
}
substrTest(){
  unset failed
  i=0
  if ! substrInStr "positive-middle" " this is positive-middlely the middle "; then failed="${failed},$((i=i+1))"; fi
  if ! substrInStr "positive-left" "positive-leftly this is left"; then failed="${failed},$((i=i+1))"; fi
  if ! substrInStr "positive-right" "this is right, positive-right"; then failed="${failed},$((i=i+1))"; fi
  if ! substrInStr "equal" "equal"; then failed="${failed},$((i=i+1))"; fi
  if substrInStr "blank" ""; then failed="${failed},$((i=i+1))"; fi
  if substrInStr "negative" "a random string without the word"; then failed="${failed},$((i=i+1))"; fi
  # if ! substrInStr "" ""; then failed="${failed},$((i=i+1))"; fi
  if ! substrInStr "newline" "a string with
    a newline and the word positive"; then failed="${failed},$((i=i+1))"; fi
  if substrInStr "newline-negative" "a string with
    a newline and no word"; then failed="${failed},$((i=i+1))"; fi
  if substrInStr "case-sensitive" "a random CASE-SENSITIVE string word"; then failed="${failed},$((i=i+1))"; fi
  if ! substrInStr "" "blank substring"; then failed="${failed},$((i=i+1))"; fi
  if substrInStr "blank string"; then failed="${failed},$((i=i+1))"; fi
  if ! substrInStr " " "white space"; then failed="${failed},$((i=i+1))"; fi

  if [ -n "${failed}" ]; then
    echo "substrInStr failed test(s) ${failed##,}!" >&2
  fi
  unset failed i
}
# substrTest


# Compare two dot-separated version numbers.
# Usage: compareVersionNum num1 '>=' num2, eg:
# if  compareVersionNum $BASH_VERSION_CLEAN '>' 4.2 ; then
compareVersionNum () {
    op=$2
    num1=$1
    num2=$3
    if [ -z "$num1" ] || [ -z "$num2" ] || [ -z "$op" ] ; then
        echo "Usage: compareVersionNum num1 '>=' num2, eg:" >&2
        echo "if  compareVersionNum $BASH_VERSION_CLEAN '>' 4.2 ; then" >&2
        return 2
    fi
    if [ "$op" != "<" ] && [ "$op" != "=" ] && [ "$op" != ">" ]; then
        echo "Invalid operator: '$op'. Valid operators are <, >, =" >&2
        return 2
    fi
  # Sort -V handles version numbers.
  # Use that and see what reaches the top of the list!
  smallestVersion="$(printf "%s\n%s" "$num1" "$num2" | sort -V | head -n1)"
  if [ "$num1" = "$num2" ]; then
    res="="
  elif [ "$num1" = "$smallestVersion" ]; then
    res="<"
  else
    res=">"
  fi

  # Use eval to unset without spoiling return code.
  eval "unset op num1 num2 smallestVersion res; [ \"$res\" = \"$op\" ]"
  return  # result of previous comparison.
}
# Basically a 1-line version of the above. Not ever called, just here to copy
# and paste where the above function can't be declared.
is1EarlierVersionThan2(){ [ "$(printf "%s\n%s" "$1" "$2" | sort -V | head -n1)" = "$1" ]; }

# Expand the variable named by $1 into its value. Works in both {ba,z}sh
# eg: a=HOME $(var_expand $a) == /home/me
var_expand() {
  if [ -z "${1-}" ] || [ $# -ne 1 ]; then
    printf 'var_expand: expected one argument\n' >&2;
    return 1;
  fi
  eval printf '%s' "\"\${$1?}\""
}

ensure_latest_shell(){
  # If available, replace shell with brew version. (More up-to-date than
  # system.) Use only if running interactively. (Done in outer case
  # statement, not here.)
  SHELL_PROGRAM="$(getShellProgram)"
  if [ -n "$SHELL" ] && [ -z "$HAVE_LOADED_SHELL" ]; then
    if [ -n "$HOMEBREW_PREFIX" ]; then
      brewshell="${HOMEBREW_PREFIX}/bin/${SHELL_PROGRAM}"
      if [ "$SHELL" = "$brewshell" ]; then
        export HAVE_LOADED_SHELL=1
      elif [ -f "$brewshell" ]; then
        export SHELL="$brewshell"
        export HAVE_LOADED_SHELL=1
        # Exec replaces the shell with the specified process.
        exec "$brewshell"
      fi
    fi
  fi
}

# Removes carriage return characters from argument file.
rmcr() {
  sed -i 's/\r$//' "$1"
}

mkbak() {
  cp "$1" "$1.bak"
}

# Replaces a file with the .bak version of itself.
disable() {
  mv "$1" "$1.bak"
}

mkcd() {
  mkdir -p "$1" && cd "$1"
}

findText(){
  text="$1"
  dir="${2:-.}"
  grep --color=auto -rn "$text" "${dir}" "${@:3}"
}

del() {
  fileToDel="$1"
  fileDir="$(dirname \"${fileToDel}\")"
  if [ $OSTYPE = 'linux-gnu' ]; then
    TRASHDIR=${HOME}/.local/share/Trash/files
  elif [[ $OSTYPE =~ 'darwin' ]]; then
    TRASHDIR=${HOME}/.Trash
  fi
  mkdir -p "${TRASHDIR}/$fileDir"
  # Safer than rm
  mv "$fileToDel" "${TRASHDIR}/$fileToDel"
}

# Delete and reclone current git directory
reclone() {
  if is_git_repository ; then
    remoteUrl=$(git config --get remote.origin.url)
    repoFolder=$(pwd)
    cd .. && rm -rf "${repoFolder}" && git clone "${remoteUrl}" && cd "${repoFolder}"
  else
    echo "Error: not a git repository ${remoteUrl}"
    return 1
  fi
}

# provides shortcuts for git cloning
git_clone() {
  if [[  "$1" =~ https://github.com ]] ; then
    git clone "$1"
  elif [[  "$1" =~ @github.com ]] ; then
    git clone "$1"
  elif [[  "$1" =~ github.com ]] ; then
    git clone https://"$1"
  else
    git clone https://github.com/"$1"
  fi
}

#TODO rceate function to try a range of terms...
term() {
  terminals="
  kitty --single-instance -d $(pwd)
  gnome-terminal
  xfce4-terminal
  terminal
  iterm2
  wsl.exe
  "
  tarray=($terminals)
  for term in "${tarray[@]}" ; do
    # if [ $(function_exists "$browser") ]; then
    function_exists "$term"
    command -v $term >/dev/null 2>&1 # deliberately only get first word of cmd. Don't quote!
    if [ "$?" -eq 0 ]; then
      "$term" "$1"
      break
    fi
  done || echo "Terminal emulator unknown" >&2
}

fopen() {
  if [ "$(uname)" = "Darwin" ]; then
    # Mac OS X, open finder.
    open $1
    return
  fi
  filebrowsers="
  xdg-open
  explorer.exe
  finder
  nautilus
  gnome-open
  caja
  dolphin
  konquerer
  nemo
  "
  fbarray=($filebrowsers)
  for browser in "${fbarray[@]}" ; do
    # if [ $(function_exists "$browser") ]; then
    function_exists "$browser"
    command -v $browser >/dev/null 2>&1
    if [ "$?" -eq 0 ]; then
      $browser "$1"
      break
    fi
  done || echo "File browser unknown" >&2
}


# Save all commands with timestamp and working dir to log file. Doesn't
# affect bash's recallable history, or speed.
# https://spin.atomicobject.com/2016/05/28/log-bash-history/
log_command() {
  if [ "$(id -u)" -ne 0 ]; then
    if [ "$SHELL_PROGRAM" = "zsh" ]; then
      _histarg="-1"
    elif [ "$SHELL_PROGRAM" = "bash" ]; then
      _histarg="1"
    else
      _histarg="1"
    fi
    _log="$HOME/.logs/shell-history-$(date "+%Y-%m-%d").log"
    if [ ! -f "$_log" ]; then touch "$_log"; fi
    echo "$(date "+%Y-%m-%d.%H:%M:%S") $(pwd) $SHELL_PROGRAM $(history ${_histarg})" >> "$_log"
  fi
  unset _histcmd _log
}

randGen() {
  for ((i = 0; i < $1; i++)); do
    echo $RANDOM
  done
}

# Detect whether the current directory is a git repository.
is_git_repository() {
  git branch > /dev/null 2>&1
}

get_git_branch() {
  BRANCH=$(git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/\1/')
  echo "${BRANCH}"
}


# If a string list (separated by a space, " ") contains the 2nd argument.
# @param $1 the list variable, eg $list = "one two"
# @param $2 the word to check
contains() {
  if  [[ $1 =~ (^|[[:space:]])$2($|[[:space:]]) ]]; then
    return 0
  else
    return 1
  fi
}

function_exists() {
    FUNCTION_NAME=$1
    declare -F "$FUNCTION_NAME" > /dev/null 2>&1
    return $?
}


# https://unix.stackexchange.com/a/48116
# Save to history when command is executed, rather than when terminal is closed.
# Then reread it.
bash_history_sync() {
  # Consider using just -a if you want commands to only be available to new shells,
  # so currently running shells have their commands listed more recently.
  # builtin history -a         #1
  # HISTFILESIZE=$HISTSIZE     #2
  # builtin history -c         #3
  # builtin history -r         #4
  builtin history -n
  builtin history -w
  builtin history -c
  builtin history -r
}
# return
# history() {                  #5
#   _bash_history_sync
#   builtin history "$@"
# }

# Call from a local repo to open the repository on github/bitbucket in browser
# Modified version of https://github.com/zeke/ghwd
repo() {
	# Figure out github repo base URL
	local base_url
	base_url=$(git config --get remote.origin.url)
	base_url=${base_url%\.git} # remove .git from end of string

	# Fix git@github.com: URLs
	base_url=${base_url//git@github\.com:/https:\/\/github\.com\/}

	# Fix git://github.com URLS
	base_url=${base_url//git:\/\/github\.com/https:\/\/github\.com\/}

	# Fix git@bitbucket.org: URLs
	base_url=${base_url//git@bitbucket.org:/https:\/\/bitbucket\.org\/}

	# Fix git@gitlab.com: URLs
	base_url=${base_url//git@gitlab\.com:/https:\/\/gitlab\.com\/}

	# Validate that this folder is a git folder
	if ! git branch 2>/dev/null 1>&2 ; then
		echo "Not a git repo!"
		exit $?
	fi

	# Find current directory relative to .git parent
	full_path=$(pwd)
	git_base_path=$(cd "./$(git rev-parse --show-cdup)" || exit 1; pwd)
	relative_path=${full_path#$git_base_path} # remove leading git_base_path from working directory

	# If filename argument is present, append it
	if [ "$1" ]; then
		relative_path="$relative_path/$1"
	fi

	# Figure out current git branch
	# git_where=$(command git symbolic-ref -q HEAD || command git name-rev --name-only --no-undefined --always HEAD) 2>/dev/null
	git_where=$(command git name-rev --name-only --no-undefined --always HEAD) 2>/dev/null

	# Remove cruft from branchname
	branch=${git_where#refs\/heads\/}

	[[ $base_url == *bitbucket* ]] && tree="src" || tree="tree"
	url="$base_url/$tree/$branch$relative_path"


	echo "Calling $(type open) for $url"

	open "$url" &> /dev/null || (echo "Using $(type open) to open URL failed." && exit 1);
}

# Get colors in manual pages
man() {
	env \
		LESS_TERMCAP_mb="$(printf '\e[1;31m')" \
		LESS_TERMCAP_md="$(printf '\e[1;31m')" \
		LESS_TERMCAP_me="$(printf '\e[0m')" \
		LESS_TERMCAP_se="$(printf '\e[0m')" \
		LESS_TERMCAP_so="$(printf '\e[1;44;33m')" \
		LESS_TERMCAP_ue="$(printf '\e[0m')" \
		LESS_TERMCAP_us="$(printf '\e[1;32m')" \
		man "$@"
}

# Compare two dot-separated version numbers.
# Usage: compareVersionNum num1 '>=' num2, eg:
# if  compareVersionNum $BASH_VERSION_CLEAN '>' 4.2 ; then
compareVersionNumNonPosix () {
  op=$2
  num1=$1
  num2=$3
  if [[ $num1 == $num2 ]]
  then
    res="="
    [ $res == "$op" ]
    return  # result of previous comparison.
fi

  local IFS=.
  local i ver1=($num1) ver2=($num2)
  # fill empty fields in ver1 with zeros
  for ((i=${#ver1[@]}; i<${#ver2[@]}; i++)); do
    ver1[i]=0
  done
  for ((i=0; i<${#ver1[@]}; i++)); do
    if [[ -z ${ver2[i]} ]]; then
      # fill empty fields in ver2 with zeros
      ver2[i]=0
    fi
    if ((10#${ver1[i]} > 10#${ver2[i]})); then
      res=">"
      break
    fi
    if ((10#${ver1[i]} < 10#${ver2[i]})); then
      res="<"
      break
    fi
  done

  # Check if result is in op (for >=). Poisix-compat.
  if substrInStr $res $op; then
    return 0
  else
    return 1
  fi
}

sshraw() {
  \ssh "$@" -t '/bin/bash --norc'
}

ssh_copy_terminfo (){
  infocmp | \ssh "$@" tic -x -o \~/.terminfo /dev/stdin
}

# {[} Exporting for ssh

generate_export_termoptions_cmd(){
  out=""
  for _option in ${TERMOPTIONS[*]}; do
    out="${out} export ${_option}=$(var_expand ${_option}); "
  done
  echo "${out}"
}

ssh_with_options(){
  local EXPORT_TERMOPTIONS_CMD=$(generate_export_termoptions_cmd)
  # Calls remote user's default shell. Unescape $SHELL to use local user's.
  \ssh -t "$@" "${EXPORT_TERMOPTIONS_CMD} exec \${SHELL} -l -s"
}
alias ssh="ssh_with_options"

mosh_with_options(){
  local EXPORT_TERMOPTIONS_CMD=$(generate_export_termoptions_cmd)
  # To get currently used shell, use getShellProgram?
  # Calls currently used shell locally on remote server. Unused.
  # stripping leading '-' and directory
  shell_base="${0##*/}"
  shell_base="${shell_base#-}"
  \mosh --server="${EXPORT_TERMOPTIONS_CMD} mosh-server || ~/.local/bin/mosh-server.sh" "$@"
}
alias mosh="mosh_with_options"

set_tmux_termoptions(){
  if is_tmux_running; then
    for _option in ${TERMOPTIONS[*]}; do
      # If attaching to a running tmux
      # session, we set a variable in tmux's global environment from
      # the containing shell. (This must be done before attaching to work!)
      # We then attach, and bash runs the refresh function.

      ## Set tmux environment
      # Check that we're in a session
      if [ ! -z "$TMUX" ]; then
        for _option in ${TERMOPTIONS[*]}; do
          # Refresh term_option shell variables by parsing tmux's global environment
          local optval="$(\tmux show-environment -g ${_option} 2>/dev/null)"
          export "${_option}"="${optval##*=}"
          unset optval
        done
      else
        # Should this go after attaching tmux or before???
        \tmux setenv -g "${_option}" "$(var_expand ${_option})"
      fi
    done
  fi
}

is_tmux_running(){
  # Check tmux is installed
  if command -v \tmux>/dev/null; then
    # Check tmux has a session running
    # if ! \tmux ls 2>&1 | grep -q "no server running"; then
    if ! substrInStr "no server running" "$(\tmux ls 2>&1)"; then
      return 0 # true
    fi
  fi
  return 1 # false
}

tmux_with_options(){
  set_tmux_termoptions
  unset PROFILE_LOADED HAVE_LOADED_BASH
  \tmux "$@"
}
alias tmux="tmux_with_options"
# {]} Exporting for ssh

ssh_reset_permissions(){
  chmod 700 $HOME/.ssh
  chmod 644 $HOME/.ssh/*
  chmod 600 $HOME/.ssh/*_rsa
  chmod 644 $HOME/.ssh/*.pub
  chmod go-w $HOME
  chown $USER $HOME/.ssh
  chown $USER $HOME/.ssh/*
}

ssh_keygen(){
  read -p "What should this key be called in the comment? " name
  ssh-keygen -t rsa -b 4096 -C "$name"
}

ssh_agent_start(){
  # Ensures only one ssh-agent will be started, even across multiple shells.
  # Queries the agent for available keys. If none can be found, it will try
  # to load the agent config from a file, and if still can't connect to the
  # agent, it will start a new one.
  ssh-add -l &>/dev/null
  if [ "$?" = 2 ]; then
    test -r ~/.ssh-agent && \
      eval "$(<~/.ssh-agent)" >/dev/null

    ssh-add -l &>/dev/null
    if [ "$?" = 2 ]; then
      (umask 066; ssh-agent > ~/.ssh-agent)
      eval "$(<~/.ssh-agent)" >/dev/null
      # ssh-add
    fi
  fi
}

lastpass_login(){
  if ! lpass status > /dev/null; then
    echo "Lastpass is not logged in."
    if [ -z "${LPUSERNAME}" ]; then
      read -r -p "Enter lastpass username \
        (set LPUSERNAME to skip prompt): " LPUSERNAME
    fi
    lpass login "$LPUSERNAME"
  fi
}

# Adds an ssh key to agent, using the passphrase in lastpass.
# Uses the extra note at the end of the .pub as the key name in lastpass.
# If no key files are given as arguments, all keys in ~/.ssh are added.
lastpass_ssh_key_add(){
  if ! command -v lpass > /dev/null; then
    echo "lastpass-cli not installed."
    ssh-add
    return
  fi
  if ! command -v ssh > /dev/null; then
    echo "openssh not installed."
    return
  fi
  if [ -z "$SSH_KEYS_ADDED" ]; then
    pubkeys="$HOME/.ssh/*.pub"
    files="${*-$pubkeys}"
    for keyfile in $files; do
      # Strip .pub from public keys...
      # keyfile="${keyfile%.*}"
      # Backup check to see if key already loaded.
      key="$(cat ${keyfile})"
      # ssh-add -L lists public keys, oops.
      # if ! ssh-add -L | grep -q -- "${key}"; then
      if ! substrInStr "${key}" "$(ssh-add -L)"; then
        # Extract the comment at the end of the pub file
        keyname=$(sed -e 's/[^=]*== //g' < "${keyfile}.pub")
        lastpass_login
        if ! lpass status; then
          echo "Lastpass not logged in. No keys added."
          return
        fi
        # Note: Indent END with tabs, not spaces, or this won't work.
        expect <<- END
          spawn ssh-add ${keyfile}
          expect "Enter passphrase"
          send "$(lpass show --field=Passphrase SSH/${keyname})\r"
          expect eof
END
        export SSH_KEYS_ADDED=1
      fi
    done
  fi
}
alias lpssh="lastpass_ssh_key_add"

choosePkgManager(){
  _options="brew yay pacman pkg apt yum"
  # Not quite posix, but will at least work in both bash and zsh.
  while read option; do
      if [ $(command -v "${option}" 2>/dev/null) ]; then
          echo "${option}"
          break
      fi
    done < <(echo $_options | tr ' ' '\n')
  unset options
}

pack(){
    if [ -n "$PACKCMD" ]; then
      cmd="$PACKCMD"
    else
      cmd="$1"
    fi
    if [ -n "$cmd" ]; then
      shift
    fi
    args="$@"
    useSudo=false
    installcmd="install"
    infocmd="info"
    refreshcmd="update"
    upgradecmd="upgrade"
    searchcmd="search"
    removecmd="remove"
    noconfirmcmd="-y"
    packcmd="$(choosePkgManager)"
    case "$packcmd" in
        brew )
            removecmd="uninstall"
            ;;
        pacman|yay)
            installcmd="-S"
            refreshcmd="-Syy"
            upgradecmd="-Syu"
            searchcmd="-Ss"
            removecmd="-R"
            noconfirmcmd="--noconfirm"
            if [ "$packcmd" = "pacman" ]; then useSudo=true; fi
            ;;
        pkg )
            # Probably termux, may be freeBSD.
            true
            ;;
        apt )
            useSudo=true
            ;;
        yum )
            useSudo=true
            ;;
    esac

    if "$useSudo"; then
        packcmd="sudo ${packcmd}"
    else
        packcmd="${packcmd}"
    fi
    if [ -n "$PACK_NOCONFIRM" ]; then
        packcmd="${packcmd} ${noconfirmcmd}"
    fi

  case "$cmd" in
    install | refresh | upgrade | search | remove | info)
      cmd="${cmd}cmd"
      # Expand cmd to get what the actual [$installcmd] is.
      $(echo "$packcmd") "$(var_expand ${cmd})" $args
      ;;
    *)
      $(echo "$packcmd") "$cmd" $args
  esac

  unset packcmd cmd installcmd refreshcmd upgradecmd searchcmd removecmd infocmd useSudo
}
alias packi="pack install"
alias packr="pack refresh"
alias packu="pack refresh && pack upgrade"
alias packs="pack search"
alias packrm="pack remove"
alias pack\?="pack info"

# kitty_termcopy(){
#   infocmp xterm-kitty | \ssh $1 tic -x -o \~/.terminfo /dev/stdin
# }
alias kitty_termcopy="kitty +kitten \ssh"

htmlCopyAsRichText(){
  if [[ "$OSTYPE" =~ "darwin" ]]; then
    textutil -stdin -format html -convert rtf -stdout | pbcopy
  elif [ "${isWSL}" = 1 ]; then  # WSL specific stuff
    pandoc.exe --standalone --from=html --to=rtf --output=- | clip.exe
  elif [[ "$OSTYPE" =~ "linux" ]] && command -v xclip > /dev/null 2>&1; then  # Linux specific stuff
    xclip -t text/html -selection clipboard
  fi
}
markdownCopyAsRichText(){
  pandoc "$@" -f markdown-smart -t html | htmlCopyAsRichText
}

viewCSV(){
  # Column by default merges empty columns, so add space before all commas to fix
  sed 's/,/ ,/g' < "$@" | column -s, -t | less -#2 -N -S
}
alias csv="viewCSV"
headCSV(){
  # Column by default merges empty columns, so add space before all commas to fix
  sed 's/,/ ,/g' < "$@" | head | column -s, -t
}
alias csvCheck="headCSV"

# {[} Theme
if [ "$TERM" = "xterm-kitty" ] && [ -z "$SSHSESSION" ]; then
  #{[} Kitty theme
  KITTY_CURRENT_THEME_FILE="${XDG_CACHE_HOME}/kitty/current_theme"

  kittyThemeMatchesColourscheme(){
    [ "$(echo $1 | tr '[:upper:]' '[:lower:]')" = "$2".conf ];
  }
  kittyColourSet(){
    arg="$1"
    if [ -z "$arg" ]; then return; fi
    # strip .conf, convert to lowercase
    colourscheme="$(basename "$(echo $arg | tr '[:upper:]' '[:lower:]')" .conf)"
    # If already set for this session, return.
    if [ "$COLOURSCHEME" = "$colourscheme" ]; then
      unset colourscheme arg; return;
    fi

    # Get current kitty theme from file
    if [ -f "${KITTY_CURRENT_THEME_FILE}" ]; then
      current_theme="$(cat "${KITTY_CURRENT_THEME_FILE}")"
    fi

    # Expand $HOME etc. KITTY_THEME_DIR set from kitty.conf.
    export KITTY_THEME_DIR="$(eval "echo ${KITTY_THEME_DIR}")"
    # If arg is same as current theme and that theme exists, make note.
    if [ "$arg" = "$current_theme" ] && \
      [ -f "${KITTY_THEME_DIR}/${current_theme}" ]; then
      theme_path="${KITTY_THEME_DIR}/${current_theme}"
    else
      KITTY_THEMES="$(ls "$KITTY_THEME_DIR" | tr '\n' ' ')"
      # Echo themes to force string splitting in zsh.
      for themeconf in $(echo $KITTY_THEMES); do
        if kittyThemeMatchesColourscheme "$themeconf" "$colourscheme"; then
          theme_path="${KITTY_THEME_DIR}/${themeconf}"
          mkdir -p "${XDG_CACHE_HOME}/kitty"
          echo "${themeconf}" >| "${KITTY_CURRENT_THEME_FILE}"
          break
        fi
      done
    fi
    if [ -n "$theme_path" ]; then
      kitty @ set-colors --all "${theme_path}" # 2>> ~/.logs/kitty.log
      export COLOURSCHEME="${colourscheme}"
    fi
    unset arg colourscheme current_theme theme_path
  }
  kittyColourReset(){
    kitty @ set-colors --all "${DOTFILES_DIR}/terminal/kitty/solarized_light.conf"
    if [ -f "${KITTY_CURRENT_THEME_FILE}" ]; then
      rm "${KITTY_CURRENT_THEME_FILE}"
    fi
  }
  theme(){
    if [ -n "$1" ]; then
      kittyColourSet "$1"
    else
      if [ -f "${KITTY_CURRENT_THEME_FILE}" ]; then
        kittyColourSet "$(cat "${KITTY_CURRENT_THEME_FILE}")"
      fi
    fi
  }
  notheme(){ kittyColourReset; }
  if [ -n "$BASH" ]; then
    complete -W "$KITTY_THEMES" kittyColourSet
    complete -W "$KITTY_THEMES" theme
  elif [ -n "$ZSH_VERSION" ]; then
    # Too hard to set up completion for, plus it isn't loaded at this stage.
    true
    # compdef _kittyColourSet kittyColourSet
  fi
  #{]} Kitty theme
else
  #{[} base16 theme
  # Added by base16, normally in bashrc. Now called by `theme` call in PROMPT_COMMAND
  if [ -z "${BASE16_SHELL}" ]; then
    BASE16_SHELL="$HOME/.config/base16-shell/"
  fi
  if [ -d "${BASE16_SHELL}" ]; then
    [ -n "$PS1" ] && \
      [ -s "$BASE16_SHELL/profile_helper.sh" ] && \
      eval "$("$BASE16_SHELL/profile_helper.sh")"
  fi

  # Check if base16 scheme has been set, set COLOURSCHEME.
  base16_colourscheme_set(){
    export COLOURSCHEME="${1:-$COLOURSCHEME}"
    theme="$(echo $COLOURSCHEME | tr '_' '-')"
    base16_theme_dir="$XDG_CONFIG_HOME/base16-shell/scripts"
    if command -v _base16 > /dev/null 2>&1; then
      _base16 "$base16_theme_dir/base16-$theme.sh" $theme
    fi
    unset theme
  }
  base16Reset(){
    unset BASE16_THEME
    unset COLOURSCHEME
    rm ~/.vimrc_background
    rm ~/.base16_theme
    reset
  }
  theme(){
    if [ -n "$1" ]; then
      base16_colourscheme_set "$1"
    else
      if [ -n "$BASE16_THEME" ]; then
        export COLOURSCHEME="base16-${BASE16_THEME}"
      fi
    fi
  }
  notheme(){ kittyColourReset; }
  #{]} base16 theme
fi
#{]} Theme

hist-search(){
  searchterm="$1"
  logs="$HOME"/.logs/shell-history-
  rg "$searchterm" "${logs}"* 2> /dev/null || \
    ag "${logs}"* "$searchterm" 2> /dev/null || \
    grep -r "$searchterm" "${logs}"*
  unset searchterm logs
}

# Gets the path of the most recently modified file in the specified or current
# directory. Excludes directories. Use like vi `mrf`
mrf(){
  dir="${1:-.}"
  echo "$dir/$(ls -ABrt1 --group-directories-first "$dir" | tail -n1)"
  unset dir
}

vim_single_plugin(){
  plugin="$1"
  shift
  if [ -n "$2" ]; then
    editor="${2}"
    shift
  else
    editor="vim"
  fi
  "$editor" -u NORC --cmd "set rtp+=$HOME/.vim/plugins/$plugin" -c "nnoremap ; :" -c "nnoremap kv <esc>" $@
  unset plugin
}

plugupdate() {
  vim +PlugUpgrade +PlugUpdate +CocUpdate +qa && zinit update && $HOME/.tmux/plugins/tpm/bin/update_plugins all
}
