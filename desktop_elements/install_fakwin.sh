#!/usr/bin/env bash
set -e
source "$DOTFILES_DIR/shell/script_functions.sh"
# Installs a shim so that shutting down works on plasma6 when not using kwin

# Needs qt6-base-dev (ubuntu) / qt6-qtbase-devel (fedora).
git_clone_commit https://github.com/DMaroo/fakwin.git f072c95390f9588f3b51a66d54490d1ddaac8385
pushd fakwin

# Variables
SERVICE_NAME="fakwin.service"
SERVICE_DIR="$HOME/.config/systemd/user"
SERVICE_PATH="$SERVICE_DIR/$SERVICE_NAME"
EXECUTABLE="$HOME/.local/bin/fakwin"

# Build the binary
mkdir -p build
cd build
cmake ..
make
mkdir -p "$(dirname "$EXECUTABLE")"
mv fakwin "$EXECUTABLE"
cd ../..

rm -rf fakwin

# Ensure the service directory exists
mkdir -p "$SERVICE_DIR"

# Create the service file
cat << EOF > "$SERVICE_PATH"
[Unit]
Description=Run fakwin binary to fix shutdowns on custom WM plasma.

# For running before shutdown
DefaultDependencies=no
Before=shutdown.target reboot.target halt.target poweroff.target

[Service]
ExecStart=$EXECUTABLE
Restart=always
Type=simple

[Install]
# run before shutdown
WantedBy=shutdown.target
EOF

# Reload systemd to recognize the new service
systemctl --user daemon-reload
# Enable the service to start on boot or before shutdown
systemctl --user enable "$SERVICE_NAME"
# Start the service immediately (optional)
systemctl --user start "$SERVICE_NAME"
