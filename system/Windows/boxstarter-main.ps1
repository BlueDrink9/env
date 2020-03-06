# $scriptdir = $PSScriptRoot
$scriptdir = get-content -path $env:APPDATA\dotfiles_win_setup_dir.txt
# colemak needs to be installed before setting lang.
. "$scriptdir\settings.ps1"
# Remove defaults before installing packages, to prevent conflicts.
. "$scriptdir\remove-default-apps.ps1"
. "$scriptdir\installPackages.ps1"
. "$scriptdir\fileAssociations.ps1"
remove-item -path $env:APPDATA\dotfiles_win_setup_dir.txt
