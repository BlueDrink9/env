# Check out https://github.com/cspotcode/PS-GuiCompletion
# Check out https://github.com/joonro/Get-ChildItem-Color
# https://www.powershellgallery.com/packages/oh-my-posh/2.0.225
# console setting tool. Easy solarized!
# https://github.com/lukesampson/concfg

# # Upgrade to pwsh 7 if available. Requires double exit though, which is
# # annoying...
# # Check if we're already in PowerShell 7 to avoid infinite loop
# if ($PSVersionTable.PSVersion.Major -lt 7) {
#     # Check if PowerShell 7 is available
#     if (Get-Command "pwsh" -ErrorAction SilentlyContinue) {
#         # Pass the current arguments and environment variables to the new session
#         pwsh -NoExit -Command {
#             # Source your Windows PowerShell profile if it exists
#             $winProfile = "$env:USERPROFILE\Documents\WindowsPowerShell\Microsoft.PowerShell_profile.ps1"
#             if (Test-Path $winProfile) {
#                 . $winProfile
#             }
#         }
#         exit  # This isn't working to exit the subshell
#     }
# }

$scriptdir = $PSScriptRoot
$DOTFILES_DIR = "$(resolve-path "$PSScriptRoot\..\..")"

. $scriptdir/settings.ps1
. $scriptdir/inputrc.ps1
. $scriptdir/aliases.ps1

# if (Get-Command "starship" -ErrorAction SilentlyContinue) {
#     # Set windowtitle, update on each line
#     function Invoke-Starship-PreCommand {
#       $current = (Split-Path -Path $PWD -Leaf)
#       $host.ui.RawUI.WindowTitle = "…/$current ($pwd)"
#     }
#     $ENV:STARSHIP_CONFIG="$DOTFILES_DIR\shell\prompts\starship.toml"
#     # Hide warnings - comment this out if debugging.
#     $ENV:STARSHIP_LOG="error"
#     &starship init powershell --print-full-init | Out-String | Invoke-Expression
# }

function Prompt {
    $exitCode = $LASTEXITCODE
    $maxDirLength = $(($Host.UI.RawUI.WindowSize.Width * 0.2))
    $dir = $PWD.Path

    # Replace user directory with ~
    $userDir = [Environment]::GetFolderPath('UserProfile')
    if ($dir.StartsWith($userDir)) {
        $dir = '~' + $dir.Substring($userDir.Length)
    }

    if ($dir.Length -gt $maxDirLength) {
        $dir = "…" + $dir.SubString($dir.Length - $maxDirLength)
    }

    $prompt = "[$dir] "
    Write-Host $prompt -NoNewline -ForegroundColor Blue

    if ($exitCode -ne 0) {
        $col = "Red"
    } else {
        $col = "Green"
    }
    Write-Host "$" -NoNewline -ForegroundColor $col
    return " "
}

# . $scriptdir/plugins.ps1
