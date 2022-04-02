# Download and run debloater
Set-ExecutionPolicy Bypass -Scope Process -Force; iex ((New-Object System.Net.WebClient).DownloadString('https://raw.githubusercontent.com/Sycnex/Windows10Debloater/master/Windows10SysPrepDebloater.ps1'))

#--- Uninstall unecessary applications that come with Windows out of the box ---
#DISABLE All BLOATWARE EXCEPT STORE
# Get-AppxPackage -AllUsers | where-object {$_.name –notlike "*store*"} | Remove-AppxPackage

# Dropbox
# Get-AppxPackage *Dropbox* | Remove-AppxPackage
# Phone
Get-AppxPackage Microsoft.CommsPhone | Remove-AppxPackage
# Phone
# Get-AppxPackage Microsoft.WindowsPhone | Remove-AppxPackage
# Mail & Calendar
# Get-AppxPackage microsoft.windowscommunicationsapps | Remove-AppxPackage
# OneNote
# Get-AppxPackage Microsoft.Office.OneNote | Remove-AppxPackage

# Get-AppxPackage | where {$_.name -Match "3dbuilder|windowsalarms|windowscommunicationapps|windowscamera|officehub|skypeapp|getstarted|zunemusic|windowsmaps|solitairecollection|bingfinance|zunevideo|bingnews|onenote|people|windowsphone|photos|windowsstore|bingsports|soundrecorder|bingweather|xboxapp"} | Remove-AppxPackage -ea 0

Get-AppxPackage Microsoft.WindowsMaps | Remove-AppxPackage
Get-AppxPackage Microsoft.3DBuilder | Remove-AppxPackage
Get-AppxPackage *Autodesk* | Remove-AppxPackage
Get-AppxPackage Microsoft.BingFinance | Remove-AppxPackage
Get-AppxPackage Microsoft.BingNews | Remove-AppxPackage
Get-AppxPackage Microsoft.BingSports | Remove-AppxPackage
Get-AppxPackage Microsoft.BingWeather | Remove-AppxPackage
Get-AppxPackage *BubbleWitch* | Remove-AppxPackage
Get-AppxPackage king.com.CandyCrush* | Remove-AppxPackage
Get-AppxPackage *Soda* | Remove-AppxPackage
Get-AppxPackage *Dell* | Remove-AppxPackage
Get-AppxPackage *Facebook* | Remove-AppxPackage
Get-AppxPackage Microsoft.WindowsFeedbackHub | Remove-AppxPackage
Get-AppxPackage Microsoft.Getstarted | Remove-AppxPackage
Get-AppxPackage *Keeper* | Remove-AppxPackage
Get-AppxPackage *MarchofEmpires* | Remove-AppxPackage
Get-AppxPackage *McAfee* | Remove-AppxPackage
# Uninstall McAfee Security App
$mcafee = gci "HKLM:\SOFTWARE\Wow6432Node\Microsoft\Windows\CurrentVersion\Uninstall" | foreach { gp $_.PSPath } | ? { $_ -match "McAfee Security" } | select UninstallString
if ($mcafee) {
	$mcafee = $mcafee.UninstallString -Replace "C:\Program Files\McAfee\MSC\mcuihost.exe",""
	Write "Uninstalling McAfee..."
	start-process "C:\Program Files\McAfee\MSC\mcuihost.exe" -arg "$mcafee" -Wait
}
# Messaging
Get-AppxPackage Microsoft.Messaging | Remove-AppxPackage
Get-AppxPackage *Minecraft* | Remove-AppxPackage
Get-AppxPackage *Netflix* | Remove-AppxPackage
Get-AppxPackage Microsoft.MicrosoftOfficeHub | Remove-AppxPackage
Get-AppxPackage Microsoft.OneConnect | Remove-AppxPackage
Get-AppxPackage Microsoft.People | Remove-AppxPackage
# Get-AppxPackage Microsoft.Windows.Photos | Remove-AppxPackage
# Get-AppxPackage Microsoft.SkypeApp | Remove-AppxPackage
Get-AppxPackage *Plex* | Remove-AppxPackage
# Get-AppxPackage Microsoft.WindowsSoundRecorder | Remove-AppxPackage
# Sticky Notes
# Get-AppxPackage Microsoft.MicrosoftStickyNotes | Remove-AppxPackage
# Xbox
# Get-AppxPackage Microsoft.XboxApp | Remove-AppxPackage
# Get-AppxPackage Microsoft.XboxIdentityProvider | Remove-AppxPackage
Get-AppxPackage *DisneyMagicKingdom* | Remove-AppxPackage
Get-AppxPackage *HiddenCityMysteryofShadows* | Remove-AppxPackage

# rovisioned Appx Packages – These (for lack of a better word) are the “package cache.” These are the manifest of what packages to install for each NEW user when they login. This is why every time your OS upgrades, you have to remove those apps again.  This is part of what is going on behind the scenes of the fabulous message “hang tight, we’re getting things ready for you” when you login to a new (or freshly upgraded) Windows 10 machines.

# How are we going to handle this?  Well, let’s just remove packages from the manifest.  Let’s go remove some of those provisioned packages:

# ##Remove Provisioned Packages 
# $appname = @( 
# "*BingWeather*" 
# "*ZuneMusic*" 
# "*ZuneVideo*" "*king*" 
# ) 
# ForEach($app in $appname){ 
# Get-AppxProvisionedPackage -Online | where {$_.PackageName -like $app} | Remove-AppxProvisione
