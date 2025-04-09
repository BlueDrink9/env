#!/usr/bin/env bash
TALON_DOTFILES_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
export talon_mani_config="${TALON_DOTFILES_DIR}/talon_plugins_mani.yml"
export talon_user_dir="$HOME/.talon/user"
# Update and clone
mani sync --sync-remotes --parallel --config $talon_mani_config
# Update
mani run update --parallel --config $talon_mani_config
