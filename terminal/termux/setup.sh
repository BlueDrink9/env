#!/usr/bin/env bash
# Allow launching of apps by name?
mkdir -p "$HOME/.termux"
downloadURLAndExtractZipTo "https://github.com/adi1090x/termux-style/raw/master/data.tar.gz" \
    "$HOME/.termux/termux-style" && \
    cp "$HOME/.termux/termux-style/solarized-light.properties" "$HOME/.termux/"
# Provide launcher support. Launch _
curl https://github.com/thewisenerd/dotfiles/blob/88d0534c67c2e93f601b1dd757cea9072eda6827/bin/zx -o /usr/bin/launch
chmod +x /usr/bin/launch
while read -r pkg; do
# for file in $(cat $($SCRIPTDIR_CMD)/packages); do
    pkg install $pkg
done < "$($SCRIPTDIR_CMD)/packages"
termux-setup-api
# Disable openssh password auth. Only allow public key.
sed -i 's/#PasswordAuthentication.*/PasswordAuthentication no/' $PREFIX/etc/ssh/sshd_config
