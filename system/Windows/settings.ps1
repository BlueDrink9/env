# Boxstarter-specific commands
Set-WindowsExplorerOptions -EnableShowHiddenFilesFoldersDrives -EnableShowFileExtensions -EnableShowRecentFilesInQuickAccess -EnableShowFrequentFoldersInQuickAccess -EnableExpandToOpenFolder -EnableShowRibbon
Disable-BingSearch
Enable-MicrosoftUpdate 
Disable-GameBarTips
Install-WindowsUpdate -AcceptEula

Set-TaskbarOptions -Lock -Dock Bottom -Combine full -AlwaysShowIconsOn
# /Boxstarter-specific commands

function createNewRegKey($path){
    if(!(Test-Path $path)){
        # This deletes the contents of the key, which is why we test it exists first.
        New-Item $path -Force;
    }
}

function Set-RegKey($path, $name, $value, $type="DWORD"){
    createNewRegKey($path)
    If ($Null -eq (Get-ItemProperty -Path $path -Name $name -ErrorAction SilentlyContinue)) {
        New-ItemProperty -Path $Path -Name $name -Value $Value -PropertyType $Type -Force
    } Else {
        Set-ItemProperty -Path $Path -Name $name -Value $Value -Force
    }
}


# Create %HOME% environment variable
[System.Environment]::SetEnvironmentVariable("HOME", "${env:HOMEDRIVE}$env:HOMEPATH", [System.EnvironmentVariableTarget]::User)

cinst --cacheLocation "$env:userprofile\AppData\Local\ChocoCache" colemak -y
# Input langs - colemak and US-nz.
# Current user, then default (which includes welcome screen).
$userpaths = @(
        "Registry::HKCU\Keyboard Layout",
        "Registry::HKEY_USERS\.DEFAULT\Keyboard Layout"
)
foreach ($userpath in $userpaths)
{
    # First strip out old values
    Remove-ItemProperty -Path "$userpath\Substitutes" -Name "*"
    Remove-ItemProperty -Path "$userpath\Preload" -Name "*"
    # Add colemak then US-nz.
    New-ItemProperty -Path "$userpath\Substitutes" -Name 'd0011409' -Value 'a0000409' -PropertyType 'String'
    New-ItemProperty -Path "$userpath\Substitutes" -Name '00001409' -Value '00000409' -PropertyType 'String'
    # Preload = default loaded layouts?
    Set-ItemProperty -Path "$userpath\Preload" -Name 1 -Value 'd0011409'
    Set-ItemProperty -Path "$userpath\Preload" -Name 2 -Value '00001409'
    # Disable switching hotkeys. 1 = alt+shift, 2 = ctrl+shift. 3 = disabled.
    Set-ItemProperty -Path "$userpath\Toggle" -Name HotKey -Value 3
    Set-ItemProperty -Path "$userpath\Toggle" -Name "Language Hotkey" -Value '3'
    Set-ItemProperty -Path "$userpath\Toggle" -Name "Layout Hotkey" -Value '3'
}
Set-WinDefaultInputMethodOverride -InputTip "1409:a0000409"



#--- Windows Settings ---
# Some from: @NickCraver's gist https://gist.github.com/NickCraver/7ebf9efbfd0c3eab72e9

# Privacy: Let apps use my advertising ID: Disable
Set-RegKey -Path "HKCU:\SOFTWARE\Microsoft\Windows\CurrentVersion\AdvertisingInfo" -Name Enabled -Type DWord -Value 0

# WiFi Sense: HotSpot Sharing: Disable
Set-regkey -Path HKLM:\Software\Microsoft\PolicyManager\default\WiFi\AllowWiFiHotSpotReporting -Name value -Type DWord -Value 0

# WiFi Sense: Shared HotSpot Auto-Connect: Disable
Set-Regkey -Path HKLM:\Software\Microsoft\PolicyManager\default\WiFi\AllowAutoConnectToWiFiSenseHotspots -Name value -Type DWord -Value 0

# Start Menu: Disable Bing Search Results
Set-Regkey -Path HKCU:\SOFTWARE\Microsoft\Windows\CurrentVersion\Search -Name BingSearchEnabled -Type DWord -Value 0
# To Restore (Enabled):
# Set-Regkey -Path HKCU:\SOFTWARE\Microsoft\Windows\CurrentVersion\Search -Name BingSearchEnabled -Type DWord -Value 1

# Disable Telemetry (requires a reboot to take effect)
# Note this may break Insider builds for your organization
Set-Regkey -Path HKLM:\SOFTWARE\Policies\Microsoft\Windows\DataCollection -Name AllowTelemetry -Type DWord -Value 0
Get-Service DiagTrack,Dmwappushservice | Stop-Service | Set-Service -StartupType Disabled

# Change Explorer home screen back to "This PC"
Set-Regkey -Path HKCU:\SOFTWARE\Microsoft\Windows\CurrentVersion\Explorer\Advanced -Name LaunchTo -Type DWord -Value 1
# Change it back to "Quick Access" (Windows 10 default)
# Set-Regkey -Path HKCU:\SOFTWARE\Microsoft\Windows\CurrentVersion\Explorer\Advanced -Name LaunchTo -Type DWord -Value 2

# Better File Explorer
Set-Regkey -Path HKCU:\Software\Microsoft\Windows\CurrentVersion\Explorer\Advanced -Name NavPaneExpandToCurrentFolder -Value 1		
Set-Regkey -Path HKCU:\Software\Microsoft\Windows\CurrentVersion\Explorer\Advanced -Name NavPaneShowAllFolders -Value 1		
Set-Regkey -Path HKCU:\Software\Microsoft\Windows\CurrentVersion\Explorer\Advanced -Name MMTaskbarMode -Value 2

# These make "Quick Access" behave much closer to the old "Favorites"
# Disable Quick Access: Recent Files
# Set-Regkey -Path HKCU:\SOFTWARE\Microsoft\Windows\CurrentVersion\Explorer -Name ShowRecent -Type DWord -Value 0
# Disable Quick Access: Frequent Folders
# Set-Regkey -Path HKCU:\SOFTWARE\Microsoft\Windows\CurrentVersion\Explorer -Name ShowFrequent -Type DWord -Value 0
# To Restore:
# Set-Regkey -Path HKCU:\SOFTWARE\Microsoft\Windows\CurrentVersion\Explorer -Name ShowRecent -Type DWord -Value 1
# Set-Regkey -Path HKCU:\SOFTWARE\Microsoft\Windows\CurrentVersion\Explorer -Name ShowFrequent -Type DWord -Value 1

# Disable the Lock Screen (the one before password prompt - to prevent dropping the first character)
Set-Regkey -Path HKLM:\SOFTWARE\Policies\Microsoft\Windows\Personalization -Name NoLockScreen -Type DWord -Value 1
# To Restore:
# Set-Regkey -Path HKLM:\SOFTWARE\Policies\Microsoft\Windows\Personalization -Name NoLockScreen -Type DWord -Value 1

# Lock screen (not sleep) on lid close
# Set-Regkey -Path 'HKLM:\SYSTEM\CurrentControlSet\Control\Session Manager\Power' -Name AwayModeEnabled -Type DWord -Value 1
# To Restore:
# Set-Regkey -Path 'HKLM:\SYSTEM\CurrentControlSet\Control\Session Manager\Power' -Name AwayModeEnabled -Type DWord -Value 0

# Use the Windows 7-8.1 Style Volume Mixer
# If (-Not (Test-Path "HKLM:\SOFTWARE\Microsoft\Windows NT\CurrentVersion\MTCUVC")) {
# 	New-Item -Path "HKLM:\SOFTWARE\Microsoft\Windows NT\CurrentVersion" -Name MTCUVC | Out-Null
# }
# Set-Regkey -Path "HKLM:\SOFTWARE\Microsoft\Windows NT\CurrentVersion\MTCUVC" -Name EnableMtcUvc -Type DWord -Value 0
# To Restore (Windows 10 Style Volume Control):
# Set-Regkey -Path "HKLM:\SOFTWARE\Microsoft\Windows NT\CurrentVersion\MTCUVC" -Name EnableMtcUvc -Type DWord -Value 1

# Disable Xbox Gamebar
# Set-Regkey -Path "HKCU:\SOFTWARE\Microsoft\Windows\CurrentVersion\GameDVR" -Name AppCaptureEnabled -Type DWord -Value 0
# Set-Regkey -Path "HKCU:\System\GameConfigStore" -Name GameDVR_Enabled -Type DWord -Value 0

# Turn off People in Taskbar
Set-Regkey -Path "HKCU:SOFTWARE\Microsoft\Windows\CurrentVersion\Explorer\Advanced\People" -Name PeopleBand -Type DWord -Value 0

# No SMB1 - https://blogs.technet.microsoft.com/filecab/2016/09/16/stop-using-smb1/
Disable-WindowsOptionalFeature -Online -FeatureName smb1protocol

# Privacy settings.
# . Logit "Disabling 'Send Microsoft info about how I write to help us improving typing and writing in the future' functionality..."
New-Item -Path 'HKCU:\SOFTWARE\Microsoft\Input' -Name 'TIPC' -Force
New-ItemProperty -Path "HKCU:\SOFTWARE\Microsoft\Input\TIPC" -Name "Enabled" -PropertyType DWORD -Value "0" -Force
# . Logit "Adjusting feedback frequency. A value of 0 indicates that user is never prompted to provide feedback about Windows 10 functionality by Microsoft."
New-Item -Path 'HKCU:\Software\Microsoft\Siuf' -Name 'Rules' -Force
New-ItemProperty -Path "HKCU:\Software\Microsoft\Siuf\Rules" -Name "PeriodInNanoSeconds" -PropertyType DWORD -Value "0" -Force
New-ItemProperty -Path "HKCU:\Software\Microsoft\Siuf\Rules" -Name "NumberOfSIUFInPeriod" -PropertyType DWORD -Value "0" -Force

#################
# Services
#################

# . Logit "Disabling Microsoft Account Sign-in Assistant Service..."
# This is needed for ms account login. Keep it.
# Set-Service wlidsvc -StartupType Disabled
# . Logit "Disabling Windows Error Reporting Service..."
Set-Service WerSvc -StartupType Disabled
# . Logit "Disabling Xbox Live Auth Manager Service..."
# Set-Service XblAuthManager -StartupType Disabled
# . Logit "Disabling Xbox Live Game Save Service..."
# Set-Service XblGameSave -StartupType Disabled
# . Logit "Disabling Xbox Live Networking Service Service..."
# Set-Service XboxNetApiSvc -StartupType Disabled
# . Logit "Disabling Xbox Accessory Management Service..."
# Set-Service XboxGipSvc -StartupType Disabled

#################
# Sched tasks
#################
# . Logit "Disabling Scheduled Tasks..."
Disable-ScheduledTask -TaskName "\Microsoft\Windows\Application Experience\Microsoft Compatibility Appraiser"
Disable-ScheduledTask -TaskName "\Microsoft\Windows\Application Experience\ProgramDataUpdater"
Disable-ScheduledTask -TaskName "\Microsoft\Windows\Application Experience\StartupAppTask"
Disable-ScheduledTask -TaskName "\Microsoft\Windows\Customer Experience Improvement Program\Consolidator"
Disable-ScheduledTask -TaskName "\Microsoft\Windows\Customer Experience Improvement Program\UsbCeip"
Disable-ScheduledTask -TaskName "\Microsoft\Windows\DiskDiagnostic\Microsoft-Windows-DiskDiagnosticResolver"
Disable-ScheduledTask -TaskName "\Microsoft\Windows\Maps\MapsToastTask"
Disable-ScheduledTask -TaskName "\Microsoft\Windows\Maps\MapsUpdateTask"
Disable-ScheduledTask -TaskName "\Microsoft\Windows\Shell\FamilySafetyMonitor"
Disable-ScheduledTask -TaskName "\Microsoft\Windows\WDI\ResolutionHost"
Disable-ScheduledTask -TaskName "\Microsoft\Windows\Windows Media Sharing\UpdateLibrary"
Disable-ScheduledTask -TaskName "\Microsoft\Windows\Autochk\Proxy"
Disable-ScheduledTask -TaskName "\Microsoft\Windows\CloudExperienceHost\CreateObjectTask"
Disable-ScheduledTask -TaskName "\Microsoft\Windows\Feedback\Siuf\DmClient"
Disable-ScheduledTask -TaskName "\Microsoft\Windows\Feedback\Siuf\DmClientOnScenarioDownload"
Disable-ScheduledTask -TaskName "\Microsoft\Windows\Shell\FamilySafetyRefreshTask"
Disable-ScheduledTask -TaskName "\Microsoft\Windows\Windows Error Reporting\QueueReporting"
Disable-ScheduledTask -TaskName "\Microsoft\XblGameSave\XblGameSaveTask"

#################
# Windows Updates
#################
# Change Windows Updates to "Notify to schedule restart"
Set-Regkey -Path HKCU:\SOFTWARE\Microsoft\WindowsUpdate\UX\Settings -Name UxOption -Type DWord -Value 1
# To Restore (Automatic):
#Set-Regkey -Path HKCU:\SOFTWARE\Microsoft\WindowsUpdate\UX\Settings -Name UxOption -Type DWord -Value 0

# Disable P2P Update downlods outside of local network
Set-Regkey -Path HKLM:\SOFTWARE\Microsoft\Windows\CurrentVersion\DeliveryOptimization\Config -Name DODownloadMode -Type DWord -Value 1
Set-Regkey -Path HKCU:\SOFTWARE\Microsoft\Windows\CurrentVersion\DeliveryOptimization -Name SystemSettingsDownloadMode -Type DWord -Value 3
# To restore (PCs on my local network and PCs on the internet)
#Set-Regkey -Path HKLM:\SOFTWARE\Microsoft\Windows\CurrentVersion\DeliveryOptimization\Config -Name DODownloadMode -Type DWord -Value 3
#Set-Regkey -Path HKCU:\SOFTWARE\Microsoft\Windows\CurrentVersion\DeliveryOptimization -Name SystemSettingsDownloadMode -Type DWord -Value 1
# To disable P2P update downloads completely:
#Set-Regkey -Path HKLM:\SOFTWARE\Microsoft\Windows\CurrentVersion\DeliveryOptimization\Config -Name DODownloadMode -Type DWord -Value 0


# Dark Theme for Windows (commenting out by default because this one's probbly a minority want)
# Note: the title bar text and such is still black with low contrast, and needs additional tweaks (it'll probably be better in a future build)
#If (-Not (Test-Path HKLM:\SOFTWARE\Microsoft\Windows\CurrentVersion\Themes\Personalize)) {
#	New-Item -Path HKLM:\SOFTWARE\Microsoft\Windows\CurrentVersion\Themes -Name Personalize | Out-Null
#}
#Set-Regkey -Path HKLM:\SOFTWARE\Microsoft\Windows\CurrentVersion\Themes\Personalize -Name AppsUseLightTheme -Type DWord -Value 0
#Set-Regkey -Path HKCU:\SOFTWARE\Microsoft\Windows\CurrentVersion\Themes\Personalize -Name AppsUseLightTheme -Type DWord -Value 0
# To Restore (Light Theme):
#Set-Regkey -Path HKLM:\SOFTWARE\Microsoft\Windows\CurrentVersion\Themes\Personalize -Name AppsUseLightTheme -Type DWord -Value 1
#Set-Regkey -Path HKCU:\SOFTWARE\Microsoft\Windows\CurrentVersion\Themes\Personalize -Name AppsUseLightTheme -Type DWord -Value 1

# POWER OPTIONS #
# -x = /change. This option only works for timeouts like these. In minutes.
powercfg -x -hibernate-timeout-ac 0
powercfg -x -hibernate-timeout-dc 90
# Screen on AC timeout
powercfg /change monitor-timeout-ac 15
# PC on AC to never sleep
powercfg /change standby-timeout-ac 0
# Screen on Battery (Laptop Only)
powercfg /change monitor-timeout-dc 5
# Sleep on Battery (Laptop Only)
powercfg /change standby-timeout-dc 10
# Get active scheme guid
$activeScheme = cmd /c "powercfg /getactivescheme"
$regEx = '(\{){0,1}[0-9a-fA-F]{8}\-[0-9a-fA-F]{4}\-[0-9a-fA-F]{4}\-[0-9a-fA-F]{4}\-[0-9a-fA-F]{12}(\}){0,1}'
$asGuid = [regex]::Match($activeScheme,$regEx).Value
$asGuid
$pwrGuid = '4f971e89-eebd-4455-a8de-9e59040e7347'
$currentPowerGuid = "$asGuid $pwrGuid"
# Get guids for specific settings with powercfg /Q $asGuid
#AC Value = While plugged in. DC Value = On Battery
# 1 = sleep, 0 = do nothing
#relative GUIDs for Lid Close settings
# $lidClosedGuid = '5ca83367-6e45-459f-a27b-476b1d01c936'
$lidClosedGuid = 'LIDACTION'
cmd /c "powercfg /setdcvalueindex $currentPowerGuid $lidClosedGuid 1"
cmd /c "powercfg /setacvalueindex $currentPowerGuid $lidClosedGuid 0"
# 1 = sleep, 3 = shut down
# $buttonGuid = '7648efa3-dd9c-4e3e-b566-50f929386280'
$buttonGuid = 'PBUTTONACTION'
cmd /c "powercfg /setdcvalueindex $currentPowerGuid $buttonGuid 3"
cmd /c "powercfg /setacvalueindex $currentPowerGuid $buttonGuid 3"
# # Number of seconds.
# $hibernateAfterGuid = '9d7815a6-7ee4-497e-8888-515a05f02364'
# $hibernateAfterGuid = 'HIBERNATEIDLE'
# # 90 mins
# cmd /c "powercfg /setdcvalueindex $currentPowerGuid $hibernateAfterGuid 1518"
# cmd /c "powercfg /setacvalueindex $currentPowerGuid $hibernateAfterGuid 0"
#apply settings
cmd /c "powercfg /s $asGuid"


# Configure "This PC" entries
$basepath1 = "HKEY_LOCAL_MACHINE\SOFTWARE"
$basepath2 = "Microsoft\Windows\CurrentVersion\Explorer\MyComputer\NameSpace"
$paths = @(
    "Registry::$basepath1\$basepath2",
    "Registry::$basepath1\Wow6432Node\$basepath2"
)
$folderIDs = @(
        # videos
        "{A0953C92-50DC-43bf-BE83-3742FED03C9C}",
        "{f86fa3ab-70d2-4fc7-9c99-fcbf05467f3a}",
        # music
        "{1CF1260C-4DD0-4ebb-811F-33C572699FDE}",
        "{3dfdf296-dbec-4fb4-81d1-6a3438bcf4de}",
        # pictures
        "{3ADD1653-EB32-4cb0-BBD7-DFA0ABB5ACCA}",
        "{24ad3ad4-a569-4530-98e1-ab02f9417aa8}",
        # 3D Objects
        "{0DB7E03F-FC29-4DC6-9020-FF41B59E513A}"
)
# Remove Videos From This PC
foreach ($path in $paths)
{
    foreach ($folderID in $folderIDs)
    {
        Remove-Item -Path "$path\$folderID"
        "$path\$folderID"
    }
}

# Pin user folder to side navigation bar
$RegKeyPath = "HKCU:\SOFTWARE\Classes\CLSID\{59031a47-3f72-44a7-89c5-5595fe6b30ee}"
$PinUser = "System.IsPinnedToNameSpaceTree"
Set-RegKey -Path $RegKeyPath -Name $PinUser -Value 1 -PropertyType DWORD -Force | Out-Null

# Remove search from taskbar
Set-Regkey -Path "HKCU:\Software\Microsoft\Windows\CurrentVersion\Search" -Name "SearchboxTaskbarMode" -Type DWord -Value 0

# Unbind shortcuts used for game bar, so I can use them with virtual desktop
# enhancer instead.
Set-Regkey -Path "HKCU:\Software\Microsoft\Windows\CurrentVersion\Search" -Name "AppCaptureEnabled" -Type DWord -Value 0

Set-Service -StartupType Automatic ssh-agent

#--- Rename the Computer ---
# Also set description to same as name.
# Requires restart, or add the -Restart flag
$computername = "BlueFizzy"
if ($env:computername -ne $computername) {
	Rename-Computer -NewName $computername -force
    Get-CimInstance -ClassName Win32_OperatingSystem | Set-CimInstance -Property @{Description = $computername}
}


# Change the default lockscreen wallpaper.
# https://abcdeployment.wordpress.com/2017/04/20/how-to-set-custom-backgrounds-for-desktop-and-lockscreen-in-windows-10-creators-update-v1703-with-powershell/
$RegKeyPath = "HKLM:\SOFTWARE\Microsoft\Windows\CurrentVersion\PersonalizationCSP"
createNewRegKey $RegKeyPath

$LockScreenPath = "LockScreenImagePath"
$LockScreenStatus = "LockScreenImageStatus"
$LockScreenUrl = "LockScreenImageUrl"

$StatusValue = "1"

# https://wallpapercave.com/wp/cETFpUH.jpg
$LockScreenImageValue = "$home\Pictures\Wallpapers\Colourful jungle river 1080.jpg"

  New-ItemProperty -Path $RegKeyPath -Name $LockScreenStatus -Value $StatusValue -PropertyType DWORD -Force | Out-Null
  New-ItemProperty -Path $RegKeyPath -Name $LockScreenUrl -Value $LockScreenImageValue -PropertyType STRING -Force | Out-Null
  New-ItemProperty -Path $RegKeyPath -Name $LockScreenPath -Value $LockScreenImageValue -PropertyType STRING -Force | Out-Null
