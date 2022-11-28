# By default, komorebi will start up $Env:UserProfile\komorebi.ahk if it exists.
# $Env:KOMOREBI_AHK_V1_EXE="$(where.exe autohotkey)"

. "$PSScriptRoot\variables.ps1"

komorebic start --await-configuration

# Enable hot reloading of changes to this file
komorebic.exe watch-configuration "enable"

# optionally, if you want a different colour for stacks of windows
# komorebic.exe active-window-border-colour [R G B] --window-kind stack
:
$gap=1
# komorebic.exe container-padding 0 <WORKSPACE_INDEX> $gap
for ($i = 0; $i -lt 10; $i++){
    komorebic.exe workspace-padding 0 $i $gap
}

komorebic.exe mouse-follows-focus disable
komorebic.exe focus-follows-mouse disable

komorebic.exe ensure-workspaces 0 4

# Configure the invisible border dimensions
komorebic.exe invisible-borders 0 0 0 0
komorebic.exe work-area-offset 0 0 14 0

# Configure the 1st workspace
# komorebic.exe workspace-name 0 0 I
# this is a nice blue colour #42A5F5
# active-window-border-colour 66 165 245 single
komorebic.exe active-window-border enable
# Hiding is faster than minimising
komorebic.exe window-hiding-behaviour hide
# Set cross-monitor move behaviour to insert instead of swap
komorebic.exe cross-monitor-move-behaviour insert

komorebic.exe resize-delta 5

# Leave till last
start-process -WindowStyle hidden autohotkey.exe `
    -ArgumentList "$PSScriptRoot\komorebi.ahk"
