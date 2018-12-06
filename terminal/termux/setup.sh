# Allow launching of apps by name?
curl https://github.com/thewisenerd/dotfiles/blob/88d0534c67c2e93f601b1dd757cea9072eda6827/bin/zx -o /usr/bin/launch
chmod +x /usr/bin/launch
for file in packages; do
    pkg install $file
done
