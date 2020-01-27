# Check out https://github.com/cspotcode/PS-GuiCompletion
# Check out https://github.com/joonro/Get-ChildItem-Color
# https://www.powershellgallery.com/packages/oh-my-posh/2.0.225
# console setting tool. Easy solarized!
# https://github.com/lukesampson/concfg

$scriptdir = $PSScriptRoot
$DOTFILES_DIR = "$PSScriptRoot\..\.."

set-location $home

. $scriptdir/settings.ps1
. $scriptdir/inputrc.ps1
. $scriptdir/aliases.ps1
. $scriptdir/plugins.ps1
