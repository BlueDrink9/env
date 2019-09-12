# $scriptdir = $PSScriptRoot
$scriptdir = get-content -path $env:APPDATA\dotfiles_win_setup_dir.txt
. "$scriptdir\remove-default-apps.ps1"
. "$scriptdir\packages.ps1"
. "$scriptdir\settings.ps1"
remove-item -path $env:APPDATA\dotfiles_win_setup_dir.txt
