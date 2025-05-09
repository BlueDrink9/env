#!/usr/bin/env bash
source "$DOTFILES_DIR/shell/script_functions.sh"

doGit() {
  gitSettings
  if [ "$LITE" = 1 ]; then
    return
  fi
  gitCredentials "$@"
}

undoGit() {
  if askQuestionYN "Remove stored git username and email?"; then
    git config --global --unset-all user.name
    git config --global --unset-all credential.https://github.com.username
    git config --global --unset-all user.email
  fi
  git config --global --unset core.excludesFile
  git config --global --unset core.attributesFile
  git config --global --unset include.path
}

gitUser() {
  if [[ ${1:-} =~ ^--?[gG](it)?-?[cC](redentials)? ]]; then
    printErr "Forcing git update."
  elif [[ "$(git config --global user.name)" =~ .+ ]] && [[ $(git config --global user.email) =~ .+ ]]; then
    printErr "$OK Git global user is set."
    printErr "Re-run with ( ./install -gu ) to force update."
    return 0
  fi

  default_user=bluedrink9
  printErr "Setting up git global user..."
  echo -ne "${Green}Enter your github username [$default_user]:${NC} "
  read -r GIT_USER
  GIT_USER="${GIT_USER:-$default_user}"
  # Blank or unset email: Use github no-reply email as a default.
  default_email="${GIT_USER}@users.noreply.github.com"
  echo -ne "${Green}Enter your git email [$default_email]:${NC} "
  read -r GIT_EMAIL
  GIT_EMAIL="${GIT_EMAIL:-$default_email}"

  printErr "${Yellow}Configuring git.$NC"
  printErr "Username: $GIT_USER"
  printErr "Email: $GIT_EMAIL"
  git config --global user.name "$GIT_USER"
  git config --global credential.https://github.com.username "$GIT_USER"
  git config --global user.email "$GIT_EMAIL"
  printErr "${Cyan}You can update your git user by entering:$NC ./install -gu"
  # Ensure for this env repo that we use bluedrink9
  git config user.name "$default_user"
  git config credential.https://github.com.username "$default_user"
  git config user.email "$default_email"
  # For this repo we usually want to push with ssh, but we always want to
  # pull with http to avoid password hassle.
  current_remote="$(git remote get-url origin)"
  git config remote.origin.pushurl ssh://git@github.com/$(basename $(dirname $current_remote))/$(basename $current_remote)

  unset default_email default_user
}

gitCredentials() {
  if [ "$ALL" == 1 ] || askQuestionYN "${Yellow}Set up Git credentials?${NC}"; then

    gitUser "$@"

    if [[ ! "$(git config --global user.name)" =~ .+ ]] || [[ ! $(git config --global user.email) =~ .+ ]]; then
      printErr "$OK Git global user is not setup correctly."
      printErr "Re-run with ( ./install -gu ) to force update."
      return 0
    fi

    case "$OSTYPE" in
      darwin*)
        # macOS
        git config --global credential.helper osxkeychain
        ;;
      linux*)
        # Linux
        if command -v git-credential-manager > /dev/null; then
          git config --global credential.helper git-credential-manager
        elif command -v git-credential-oath > /dev/null; then
          git config --global credential.helper git-credential-oath
        else
          git config --global credential.helper "cache --timeout 120"
        fi
        ;;
      cygwin*|msys*|microsoft*|mingw*)
        # Windows
        git config --global credential.helper manager
        ;;
      *)
        # Unknown OS
        printErr "${Red}Unsupported operating system${NC}"
        exit 1
        ;;
    esac

    if command -v gh > /dev/null && askQuestionYN "Authenticate https with Github?"; then
      echo "Authenticating via github. Got to github.com/login/devices."
      BROWSER=false gh auth login --git-protocol https --hostname github.com --web && gh auth setup-git
    fi

  fi
}

gitSettings() {
  printErr "Enabling custom git setup..."
  # Include is only supported on git versions > 1.7.10
  # (but 2.0 is quite standard anyway).
  # TODO check this, and also append contents if not.
  git config --global include.path "$($SCRIPTDIR_CMD)/gitconfig"
  git config --global core.excludesfile "$($SCRIPTDIR_CMD)/gitignore"
  git config --global core.attributesfile "$($SCRIPTDIR_CMD)/gitattributes"
}

# If directly run instead of sourced, do all
if [ ! "${BASH_SOURCE[0]}" != "${0}" ]; then
  doGit "$@"
fi
