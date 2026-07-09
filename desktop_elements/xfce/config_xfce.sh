#!/usr/bin/env bash
source "$DOTFILES_DIR/shell/script_functions.sh"
source "$DOTFILES_DIR/shell/functions.sh"

set -xe

# Helper: create or update key
set_prop() {
    local channel="$1"
    local prop="$2"
    local type="$3"
    local value="$4"

    if xfconf-query -c "$channel" -p "$prop" >/dev/null 2>&1; then
        xfconf-query -c "$channel" -p "$prop" -s "$value"
    else
        xfconf-query -c "$channel" -p "$prop" --create -t "$type" -s "$value"
    fi
}

# Just sets bottom padding for workspace, for now.
PANEL_HEIGHT=35

profile="$($SCRIPTDIR_CMD)/panel_config.tar.bz2"
xfce4-panel-properties load "$profile"

## WM
xfconf-query -c xfce4-session -p /sessions/Failsafe/Client0_Command -t string -sa xfsettingsd
xfconf-query -c xfce4-session -p /sessions/Failsafe/Client1_Command -t string -sa bspwm

### KEYBOARD LAYOUTS ###
set_prop "keyboards" "/Default/XkbLayout"   "string" "us"
set_prop "keyboards" "/Default/XkbVariant"  "string" ""
set_prop "keyboards" "/Default/XkbOptions"  "string" "caps:escape"

### POINTER SETTINGS ###
set_prop "pointers" "/Default/Acceleration" "double" "1.0"

### POWER MANAGER ###
set_prop 'xfce4-screensaver' '/lock/saver-activation/enabled' 'bool' 'false'
set_prof 'xfce4-power-manager' '/xfce4-power-manager/sleep-button-action' 'uint' '1'
set_prof 'xfce4-power-manager' '/xfce4-power-manager/inactivity-on-ac' 'uint' '60'
set_prof 'xfce4-power-manager' '/xfce4-power-manager/dpms-on-ac-sleep' 'uint' '6'
set_prof 'xfce4-screensaver' '/saver/mode'  int '0'
set_prop "xfce4-power-manager" "/xfce4-power-manager/lid-action-on-ac"                  int "0"
set_prop "xfce4-power-manager" "/xfce4-power-manager/brightness-level-on-ac"            int "80"
set_prop "xfce4-power-manager" "/xfce4-power-manager/brightness-level-inactivity-on-ac" int "40"
set_prop "xfce4-power-manager" "/xfce4-power-manager/blank-on-ac"                       int "10"

set_prop "xfce4-power-manager" "/xfce4-power-manager/lid-action-on-battery"                  int "1"
set_prop "xfce4-power-manager" "/xfce4-power-manager/brightness-level-on-battery"            int "60"
set_prop "xfce4-power-manager" "/xfce4-power-manager/brightness-level-inactivity-on-battery" int "30"
set_prop "xfce4-power-manager" "/xfce4-power-manager/blank-on-battery"                       int "5"

set_prop "xfce4-power-manager" "/xfce4-power-manager/critical-power-action" int "2"

### PANEL ###
set_prop "xfce4-panel" "/panels/1/position"          "string" "p=6;x=0;y=0"
set_prop "xfce4-panel" "/panels/1/size"              int "32"
set_prop "xfce4-panel" "/panels/1/length"            int "100"
set_prop "xfce4-panel" "/panels/1/autohide-behavior" int "0"

### SESSION ###
set_prop "xfce4-session" "/compat/TerminalEmulator" "string" "alacritty"

### THUNAR ###
set_prop "thunar" "/misc-show-full-path"          "bool" "true"
set_prop "thunar" "/misc-volume-show-progress"    "bool" "true"
set_prop "thunar" "/misc-open-new-window-as-tab"  "bool" "true"

### --------------------------------------------------------------
###  Fonts
### --------------------------------------------------------------

xfconf-query -c xsettings -p /Gtk/FontName -s "SF Pro 10"
xfconf-query -c xsettings -p /Gtk/MonospaceFontName -s "SauceCodePro NF 10"


### --------------------------------------------------------------
###  Keyboard layout switching
###  KDE: win+space toggles; XFCE: layout switching handled by xfce4-keyboard-settings
### --------------------------------------------------------------

xfconf-query -c keyboards -p /Default/XkbLayout -s "us,nz"
xfconf-query -c keyboards -p /Default/XkbVariant -s "colemak,"
xfconf-query -c keyboards -p /Default/XkbOptions -s "grp:win_space_toggle,grp_led:caps"


### --------------------------------------------------------------
###  Power settings
###  Plasma ⇒ PowerDevil; XFCE ⇒ xfce4-power-manager
### --------------------------------------------------------------

# AC behaviour
xfconf-query -c xfce4-power-manager -p /xfce4-power-manager/lid-action-on-ac -s 0     # do nothing
xfconf-query -c xfce4-power-manager -p /xfce4-power-manager/power-button-action -s 2 # shut down
xfconf-query -c xfce4-power-manager -p /xfce4-power-manager/brightness-level-on-ac -s 100
xfconf-query -c xfce4-power-manager -p /xfce4-power-manager/brightness-level-inactivity-on-ac -s 70
xfconf-query -c xfce4-power-manager -p /xfce4-power-manager/blank-on-ac -s 2         # minutes, closest to dimDisplay

# Battery behaviour
xfconf-query -c xfce4-power-manager -p /xfce4-power-manager/lid-action-on-battery -s 1 # sleep
xfconf-query -c xfce4-power-manager -p /xfce4-power-manager/brightness-level-on-battery -s 70
xfconf-query -c xfce4-power-manager -p /xfce4-power-manager/brightness-level-inactivity-on-battery -s 70
xfconf-query -c xfce4-power-manager -p /xfce4-power-manager/blank-on-battery -s 1      # minutes

# Low battery → hibernate (closest available)
xfconf-query -c xfce4-power-manager -p /xfce4-power-manager/critical-power-action -s 2


### --------------------------------------------------------------
###  Mouse settings
### --------------------------------------------------------------

# Set pointer acceleration profile (flat vs adaptive)
xfconf-query -c pointers -p /Default/Acceleration -s 0.1


### --------------------------------------------------------------
###  Panel setup (very approximate)
### --------------------------------------------------------------

PANEL=1

# Create panel if missing
if ! xfconf-query -c xfce4-panel -p /panels -l | grep -q "^/panels/$PANEL$"; then
    xfconf-query -c xfce4-panel -p /panels -s "$PANEL" --type string --create
fi

# Basic panel properties
xfconf-query -c xfce4-panel -p /panels/$PANEL/position -s "p=6;x=0;y=0"   # bottom
xfconf-query -c xfce4-panel -p /panels/$PANEL/size -s 44
xfconf-query -c xfce4-panel -p /panels/$PANEL/length -s 100
xfconf-query -c xfce4-panel -p /panels/$PANEL/autohide-behavior -s 0

###  Add plugins (approximation of KDE panel widgets)
PLUGINS=(
    "whiskermenu"
    "pager"
    "separator"
    "tasklist"
    "separator"
    "notification-plugin"
    "clock"
)

# Clear existing plugins
xfconf-query -c xfce4-panel -p /panels/$PANEL/plugin-ids -r

# Add plugins in order
PLUGIN_ID=50
IDS=""
for p in "${PLUGINS[@]}"; do
    xfconf-query -c xfce4-panel -p /plugins/plugin-$PLUGIN_ID -n -t string -s "$p"
    IDS="$IDS $PLUGIN_ID"
    PLUGIN_ID=$(( PLUGIN_ID + 1 ))
done

xfconf-query -c xfce4-panel -p /panels/$PANEL/plugin-ids -s "${IDS}" --type string --create


### --------------------------------------------------------------
###  Default apps
### --------------------------------------------------------------

xdg-mime default firefox.desktop x-scheme-handler/http
xdg-mime default firefox.desktop x-scheme-handler/https

xfconf-query -c xfce4-session -p /compat/TerminalEmulator -s "kitty"


### --------------------------------------------------------------
###  File manager settings (Thunar)
###  Dolphin → Thunar approximation
### --------------------------------------------------------------

# Show full path in titlebar
xfconf-query -c thunar -p /misc-show-full-path -s true

# Show tooltips
xfconf-query -c thunar -p /misc-volume-show-progress -s true

# Use tabs
xfconf-query -c thunar -p /misc-open-new-window-as-tab -s true


### --------------------------------------------------------------
###  Shortcuts
### --------------------------------------------------------------

xfconf-query --create -c 'xfce4-keyboard-shortcuts' -p '/commands/custom/<Alt>space' --type 'string' --set 'xfce4-appfinder -c'
xfconf-query --create -c 'xfce4-keyboard-shortcuts' -p '/commands/custom/<Primary><Alt>l' --type 'string' --set 'xflock4'
xfconf-query --reset -c 'xfce4-keyboard-shortcuts' -p '/commands/custom/<Super>e'

xfconf-query --create -c 'xsettings' -p '/Net/ThemeName' --type 'string' --set 'Breeze'
xfconf-query --create -c 'xsettings' -p '/Net/IconThemeName' --type 'string' --set 'candy-icons'

xfconf-query --create -c 'xfce4-notifyd' -p '/date-time-custom-format' --type 'string' --set '%a %H:%M:%S'
xfconf-query --create -c 'thunar' -p '/misc-single-click' --type 'bool' --set 'false'
xfconf-query --create -c 'thunar' -p '/misc-middle-click-in-tab' --type 'bool' --set 'true'
xfconf-query --create -c 'thunar' -p '/misc-full-path-in-tab-title' --type 'bool' --set 'true'
xfconf-query --create -c 'thunar' -p '/last-restore-tabs' --type 'bool' --set 'true'
xfconf-query --create -c 'thunar' -p '/misc-show-delete-action' --type 'bool' --set 'true'

xfconf-query --create -c 'xfce4-session' -p '/compat/LaunchKDE' --type 'bool' --set 'true'


## Launcher
xfconf-query --create -c 'xfce4-appfinder' -p '/single-click-execute' --type 'bool' --set 'true'
xfconf-query --create -c 'xfce4-appfinder' -p '/icon-view' --type 'bool' --set 'true'
xfconf-query --create -c 'xfce4-appfinder' -p '/sort-by-frecency' --type 'bool' --set 'true'
xfconf-query --create -c 'xfce4-appfinder' -p '/text-beside-icons' --type 'bool'--set 'true'
xfconf-query --create -c 'xfce4-appfinder' -p '/close-on-focus-lost' --type 'bool' --set 'true'

# Margin
xfconf-query --create -c 'xfwm4' -p '/general/margin_bottom' --type int--set '$PANEL_HEIGHT'
