#!/usr/bin/env bash
source "$DOTFILES_DIR/bash/script_functions.sh"

mkdir -p "$HOME/.termux"
downloadURLAndExtractZipTo "https://github.com/adi1090x/termux-style/raw/master/data.tar.gz" \
    "$HOME/.termux/termux-style" && \
    cp "$HOME/.termux/termux-style/solarized-light.properties" "$HOME/.termux/"

# Allow launching of apps by name?
# Provide launcher support. Launch _
curl https://github.com/thewisenerd/dotfiles/blob/88d0534c67c2e93f601b1dd757cea9072eda6827/bin/zx -o /usr/bin/launch
chmod +x /usr/bin/launch

# Disable openssh password auth. Only allow public key.
sed -i 's/#PasswordAuthentication.*/PasswordAuthentication no/' $PREFIX/etc/ssh/sshd_config

mkdir -p $HOME/.shortcuts
echo source "$($SCRIPTDIR_CMD)/vim" > "$HOME/.shortcuts/vim"
mkdir -p $HOME/bin
echo source "$HOME/.shortcuts/vim" > "$HOME/bin/termux-file-editor"

echo Check https://android.stackexchange.com/questions/37/how-do-i-change-the-name-of-my-android-device to alter the hostname
