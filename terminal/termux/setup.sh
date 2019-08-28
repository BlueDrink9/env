#!/usr/bin/env bash
source "$DOTFILES_DIR/bash/script_functions.sh"

shebang="#!$PREFIX/usr/bin/env bash"

mkdir -p "$HOME/.termux"
downloadURLAndExtractGzTo "https://github.com/adi1090x/termux-style/raw/master/data.tar.gz" \
    "$HOME/.termux/termux-style" && \
    cp "$HOME/.termux/termux-style/solarized-light.properties" "$HOME/.termux/"

# Allow launching of apps by name?
# Provide launcher support. Launch _
curl https://github.com/thewisenerd/dotfiles/blob/88d0534c67c2e93f601b1dd757cea9072eda6827/bin/zx -o /usr/bin/launch
chmod +x /usr/bin/launch

# Disable openssh password auth. Only allow public key.
sed -i 's/#PasswordAuthentication.*/PasswordAuthentication no/' "$PREFIX/etc/ssh/sshd_config"

# Use tergent as ssh-agent. Should override default.
mkdir -p "$HOME/.local/usr/bin"
printf "%s\ntergent" "${shebang}" > "$HOME/.local/usr/bin/ssh-agent"
chmod +x "$HOME/.local/usr/bin/"*

# Tasks are run in the background (termux isn't launched).
mkdir -p "$HOME/.shortcuts/tasks"
echo "$shebang" > "$HOME/.shortcuts/vim"
echo source "$($SCRIPTDIR_CMD)/vim" >> "$HOME/.shortcuts/vim"
printf "%s\nsshd" "${shebang}" > "$HOME/.shortcuts/tasks/sshd"
chmod +x "$HOME/.shortcuts/"*
mkdir -p "$HOME/bin"
echo "$shebang" > "$HOME/bin/termux-file-editor"
echo source "$HOME/.shortcuts/vim" >> "$HOME/bin/termux-file-editor"

echo Check https://android.stackexchange.com/questions/37/how-do-i-change-the-name-of-my-android-device to alter the hostname
unset shebang
