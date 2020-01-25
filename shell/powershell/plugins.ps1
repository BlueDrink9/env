Set-PSRepository -Name PSGallery -InstallationPolicy Trusted
function Plugin {
  param($module)
    If(Get-Module -ListAvailable -Name "$module"){
      Import-Module $module
    } Else {
      Install-Module $module -AllowClobber -Scope CurrentUser
      Import-Module $module
    }
}

Plugin GuiCompletion
Install-GuiCompletion -Key Tab

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

