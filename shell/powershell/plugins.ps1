Set-PSRepository -Name PSGallery -InstallationPolicy Trusted
function Plugin {
  param($module)
    try {
      Import-Module $module -ErrorAction Stop
    } catch [System.IO.FileNotFoundException] {
      Install-Module $module -AllowClobber -Scope CurrentUser
    }
}

Plugin GuiCompletion
Install-GuiCompletion -Key Tab

Plugin TabExpansionPlusPlus
Plugin PSUtil
Plugin ZLocation

# Coloured LS output
Plugin Get-ChildItemColor
If (-Not (Test-Path Variable:PSise)) {  # Only run this in the console and not in the ISE
    Set-Alias l Get-ChildItem -option AllScope
    Set-Alias ls Get-ChildItemColorFormatWide -option AllScope
    $Global:GetChildItemColorVerticalSpace = 1 # Powershell default is 2.
}

Plugin posh-git
Plugin oh-my-posh
Set-Theme Agnoster

