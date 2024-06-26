# By default, komorebi will start up $Env:UserProfile\komorebi.ahk if it exists.
# $Env:KOMOREBI_AHK_V1="$(where autohotkey)"
$configDir="$Env:KOMOREBI_CONFIG_HOME"
$logFile="$env:UserProfile/.logs/komorebi.log"
try {
    Stop-Transcript | out-null
} catch {}
Start-Transcript -path $logFile -force

# komorebic start --await-configuration
start-process -WindowStyle hidden komorebi
sleep 1

start-process -WindowStyle hidden autohotkey64 `
    -ArgumentList "$PSScriptRoot\komorebi.ahk"

. "$PSScriptRoot\settings.ps1"
# source the generated Crowd-sourced app configs.
# Source in powershell because autokey's run is quite slow.
. "$configDir\komorebi.generated.ps1"

# komorebic complete-configuration
Stop-Transcript
