# Check out https://github.com/cspotcode/PS-GuiCompletion
# Check out https://github.com/joonro/Get-ChildItem-Color
# https://www.powershellgallery.com/packages/oh-my-posh/2.0.225
# console setting tool. Easy solarized!
# https://github.com/lukesampson/concfg

$scriptdir = $PSScriptRoot
$DOTFILES_DIR = "$(resolve-path "$PSScriptRoot\..\..")"

# set-location $home

. $scriptdir/settings.ps1
. $scriptdir/inputrc.ps1
. $scriptdir/aliases.ps1
if (Get-Command "starship" -ErrorAction SilentlyContinue) {
    # Set windowtitle, update on each line
    function Invoke-Starship-PreCommand {
      $current = (Split-Path -Path $PWD -Leaf)
      $host.ui.RawUI.WindowTitle = "…/$current ($pwd)"
    }
    $ENV:STARSHIP_CONFIG="$DOTFILES_DIR\shell\prompts\starship.toml"
    Invoke-Expression (&starship init powershell)
}

. $scriptdir/plugins.ps1
