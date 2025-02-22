# Need to set this for mani to pick it up.
$env:talon_user_dir = "$env:APPDATA\talon\user"
$mani_dir = "$env:LOCALAPPDATA\bin"
$mani_exe = "$mani_dir\mani.exe"
$talon_mani_config = "$PSScriptRoot\talon_plugins_mani.yml"
if (!(Test-Path $mani_dir)) {
    New-Item -ItemType Directory -Path $mani_dir | Out-Null
}

# Download mani.exe if it does not exist
if (!(Test-Path $mani_exe)) {
    $zip_path = "$env:TEMP\mani.zip"
    $url = "https://github.com/alajmo/mani/releases/download/v0.30.0/mani_0.30.0_windows_amd64.zip"
    Write-Host "Downloading mani.exe..."
    Invoke-WebRequest -Uri $url -OutFile $zip_path

    Write-Host "Extracting mani.exe..."
    Expand-Archive -Path $zip_path -DestinationPath $mani_dir -Force
    Remove-Item $zip_path
}

# Run mani commands
& $mani_exe sync --parallel --config $talon_mani_config
& $mani_exe run update --parallel --config $talon_mani_config

