#!/usr/bin/env bash
source "$DOTFILES_DIR/shell/script_functions.sh"

# env doesn't pick up properly for shortcuts
shebang="#!/bin/bash"
binDir="$HOME/.local/bin"
mkdir -p "$binDir"

mkdir -p "$HOME/.termux"
downloadURLAndExtractGzTo "https://github.com/adi1090x/termux-style/raw/master/data.tar.gz" \
    "$HOME/.termux/termux-style" && \
    cp "$HOME/.termux/termux-style/solarized-light.properties" "$HOME/.termux/"

# Allow launching of apps by name?
# Provide launcher support. Launch _
curl https://github.com/thewisenerd/dotfiles/blob/88d0534c67c2e93f601b1dd757cea9072eda6827/bin/zx -o "$binDir"

# Disable openssh password auth. Only allow public key.
sed -i 's/#PasswordAuthentication.*/PasswordAuthentication no/' "$PREFIX/etc/ssh/sshd_config"

# Set up fingerprint ssh auth. Need to have existing key already copied in.
termux-keystore generate 'default' -a RSA -s 4096 -u 10
ln -s "$($SCRIPTDIR_CMD)/termux-ssh-askpass" "$binDir"/termux-ssh-askpass
# Interactive (requires typing in old passphrase). Changes it to new one.
ssh-keygen -p -f ~/.ssh/id_rsa \
    -N "$(sh termux-ssh-askpass ~/.ssh/id_rsa)"


# Tasks are run in the background (termux isn't launched).
mkdir -p "$HOME/.shortcuts/tasks"
echo "$shebang" > "$HOME/.shortcuts/vim"
echo source "$($SCRIPTDIR_CMD)/vim" >> "$HOME/.shortcuts/vim"
printf "%s\nsshd" "${shebang}" > "$HOME/.shortcuts/tasks/sshd"
chmod +x "$HOME/.shortcuts/"*
mkdir -p "$HOME/bin"  # Bin dir needed for tasks?
echo "$shebang" > "$HOME/bin/termux-file-editor"
echo source "$HOME/.shortcuts/vim" >> "$HOME/bin/termux-file-editor"

# Silence banner
touch ~/.hushlogin

# If this has already been done, it asks if you want to recreate.
yes 'n' | termux-setup-storage > /dev/null
ln -s "$HOME/storage/shared/Work" "$HOME/Work"

chmod +x "$binDir"/*

echo Check https://android.stackexchange.com/questions/37/how-do-i-change-the-name-of-my-android-device to alter the hostname
unset shebang
