# By default, komorebi will start up $Env:UserProfile\komorebi.ahk if it exists.
# $Env:KOMOREBI_AHK_V1="$(where autohotkey)"

komorebic start --await-configuration

# Enable hot reloading of changes to ~/komorebi.ahk
# komorebic watch-configuration "enable"

. "$PSScriptRoot\settings.ps1"
# source the generated Crowd-sourced app configs.
# Source in powershell because autokey's run is quite slow.
. "$env:UserProfile\.config\komorebi\komorebi.generated.ps1"

# Leave till last
start-process -WindowStyle hidden autohotkey `
    -ArgumentList "$PSScriptRoot\komorebi.ahk"

komorebic complete-configuration
