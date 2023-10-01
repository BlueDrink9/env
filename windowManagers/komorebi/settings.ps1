
. "$PSScriptRoot\variables.ps1"

# optionally, if you want a different colour for stacks of windows
# komorebic active-window-border-colour [R G B] --window-kind stack

$border_width=7
$inner_gap=10  # container
$outer_gap=-$inner_gap  # workspace
# komorebic container-padding 0 <WORKSPACE_INDEX> $gap
$n_monitors=4
$n_workspaces=6
# Have to set for every workspace and monitor, ugh
for ($mon = 0; $mon -lt $n_monitors; $mon++){
    komorebic ensure-workspaces $mon $n_workspaces
    for ($ws = 0; $ws -lt $n_workspaces; $ws++){
        komorebic workspace-padding $mon $ws -- $outer_gap
        komorebic container-padding $mon $ws $inner_gap
    }
}

# Configure the invisible border dimensions
# komorebic invisible-borders 7 0 27 10
$border_width=7
komorebic global-work-area-offset $border_width `
    $border_width (2*$border_width) (2*$border_width)

komorebic focus-follows-mouse disable
# Monitor and workspace switching relies on mouse position. Plus, dragging tabs
# doesn't immediately retile and make it hard to return the tab like in bspwm,
# so this setting makes sense.
komorebic mouse-follows-focus enable

komorebic active-window-border enable
komorebic active-window-border-width $border_width
komorebic active-window-border-offset -- -3
# Automatically set the window border to the triadic complement of the window bar colour.
# Use the triadic because it doesn't contrast as much as the direct complement
$windowbarAccentColor = Get-ItemProperty -Path "HKCU:\Software\Microsoft\Windows\DWM"
$windowbarAccentColorHex = '{0:x}' -f $windowbarAccentColor.ColorizationColor
$windowbarAccentColorRGB =  2,4,6 | ForEach-Object {
    [Convert]::ToInt32($windowbarAccentColorHex.Substring($_, 2), 16)
};
# Calculate triadic complement by rotating 120 degrees (moving each colour along one)
$accentTriadic1 = $windowbarAccentColorRGB[1..2] + $windowbarAccentColorRGB[0]
$accentTriadic2 = $windowbarAccentColorRGB[2..2] + $windowbarAccentColorRGB[0..1]
# Configure the 1st workspace
# komorebic workspace-name 0 0 I
# this is a nice blue colour #42A5F5
# komorebic active-window-border-colour 66 165 245  #42A5F5
komorebic active-window-border-colour @accentTriadic2


# Hiding is faster than minimising
komorebic window-hiding-behaviour hide
# Set cross-monitor move behaviour to insert instead of swap
komorebic cross-monitor-move-behaviour insert

komorebic resize-delta 5

