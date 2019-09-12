# $scriptdir = $PSScriptRoot
$scriptdir get-content -path $TMP\dotfiles_win_setup_dir.txt
. "$scriptdir\packages.ps1"
. "$scriptdir\remove-default-apps.ps1"
. "$scriptdir\settings.ps1"
