function Plugin($module) {
    try {
      Import-Module $module -ErrorAction Stop
    } catch [System.IO.FileNotFoundException] {
      Install-Module $module -AllowClobber -Scope CurrentUser
    }
}

Plugin ZLocation  # Also provides startup time info.
Plugin PSReadline  # Included by default after v3.

Plugin TabExpansionPlusPlus  # A little heavy.
# Plugin PSUtil  # Hurts startup time a lot
# Command-line intellisense based on PowerShell auto-completion
Plugin CompletionPredictor

# Coloured LS output
Plugin Get-ChildItemColor
If (-Not (Test-Path Variable:PSise)) {  # Only run this in the console and not in the ISE
    Set-Alias l Get-ChildItem -option AllScope
    Set-Alias ls Get-ChildItemColorFormatWide -option AllScope
    $Global:GetChildItemColorVerticalSpace = 1 # Powershell default is 2.
}

# Git autocomplete
Plugin posh-git

