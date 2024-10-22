#!/bin/sh

# set -e

CWD="$DOTFILES_DIR/editors/vscode"

# Function to create symbolic link if possible, or copy with replacement otherwise (e.g. if file already exists)
force_link() {
    target=$1
    linkname=$2
    rm -f "$linkname"
    if ln -sf "$target" "$linkname"; then
        return 0
    else
        cp -f "$target" "$linkname"
    fi
}

# Loop through the different config paths for VS Code and VSCodium
config_paths="$HOME/.config/Code/User $HOME/.config/VSCodium/User"

# Create symbolic links or copy files for each path
for config_path in $config_paths; do
    mkdir -p "$config_path"
    force_link "$CWD/settings.json" "$config_path/settings.json"
    force_link "$CWD/keybindings.json" "$config_path/keybindings.json"
    force_link "$CWD/../vim/runtimepath/snippets" "$config_path/snippets"
done

# Enable Microsoft extension gallery
mkdir -p "$HOME/.config/VSCodium"
cat > "$HOME/.config/VSCodium/product.json" <<EOL
{
  "extensionsGallery": {
    "serviceUrl": "https://marketplace.visualstudio.com/_apis/public/gallery",
    "itemUrl": "https://marketplace.visualstudio.com/items",
    "cacheUrl": "https://vscode.blob.core.windows.net/gallery/index",
    "controlUrl": ""
  }
}
EOL


# For some reason setting project.json isn't changing the gallery, but these do

export VSCODE_GALLERY_SERVICE_URL='https://marketplace.visualstudio.com/_apis/public/gallery'
export VSCODE_GALLERY_CACHE_URL='https://vscode.blob.core.windows.net/gallery/index'
export VSCODE_GALLERY_ITEM_URL='https://marketplace.visualstudio.com/items'
export VSCODE_GALLERY_CONTROL_URL=''
export VSCODE_GALLERY_RECOMMENDATIONS_URL=''



echo "Installing extensions"
extensions=$(cat "$CWD/extensions.txt")

editors="code codium"
available_editors=""

# Loop through the editors list and remove the ones that are not available
for editor in $editors; do
    if command -v "$editor" >/dev/null 2>&1; then
        available_editors="$available_editors $editor"
    fi
done

# Install extensions in the available editors
for editor in $available_editors; do
    installed_extensions=$("$editor" --list-extensions)
    to_install=$(echo "$extensions" | grep -v -x -F "$installed_extensions")

    echo "Installing these in ${editor}: $to_install"
    for extension_id in $to_install; do
        set +e
        "$editor" --install-extension "$extension_id"
        set -e
    done
    echo "Extensions installation ended in $editor."
done

if [ -z "$available_editors" ]; then
    echo "None of the specified editors ($editors) were found."
fi
