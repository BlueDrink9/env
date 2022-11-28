. "$PSScriptRoot\variables.ps1"

$configDir="$Env:KOMOREBI_CONFIG_HOME"
mkdir -p "$configDir" 2> $null
$sourceFile = $(New-TemporaryFile)
remove-item "$sourceFile" | out-null
Invoke-WebRequest -Uri "https://raw.githubusercontent.com/LGUG2Z/komorebi-application-specific-configuration/master/applications.yaml" `
-OutFile "$sourceFile"

# Generate application-specific config in config dir
komorebic.exe ahk-app-specific-configuration "$sourceFile" $PSScriptRoot\application_specific_config_overrides.yaml

pushd "$configDir"
# generates configDir\komorebi.generated.ahk
komorebic.exe ahk-library
popd
