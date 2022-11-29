# By default, komorebi will start up $Env:UserProfile\komorebi.ahk if it exists.
# $Env:KOMOREBI_AHK_V1_EXE="$(where.exe autohotkey)"

. "$PSScriptRoot\variables.ps1"

komorebic start --await-configuration

# Enable hot reloading of changes to this file
komorebic.exe watch-configuration "enable"

# optionally, if you want a different colour for stacks of windows
# komorebic.exe active-window-border-colour [R G B] --window-kind stack

$inner_gap=10  # container
$outer_gap=0  # workspace
# komorebic.exe container-padding 0 <WORKSPACE_INDEX> $gap
$n_mon=4
$n_ws=14
for ($mon = 0; $mon -lt $n_mon; $mon++){
    for ($ws = 0; $ws -lt $n_ws; $ws++){
        komorebic.exe workspace-padding $mon $ws $outer_gap
        komorebic.exe container-padding $mon $ws $inner_gap
    }
}

# Configure the invisible border dimensions
komorebic.exe invisible-borders 7 0 27 10
komorebic.exe work-area-offset 0 0 $n_ws 0

komorebic.exe mouse-follows-focus disable
komorebic.exe focus-follows-mouse disable

# not sure I like their workspace implementation. Might just stick to using virtual desktops.
komorebic.exe ensure-workspaces 0 1

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
