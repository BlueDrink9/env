# Run this line manually beforehand to allow this script to run, which will
# elevate itself then handle the rest.
# Set-ExecutionPolicy -scope currentuser Unrestricted -Force
#
# To be run from administrative shell
##Requires -RunAsAdministrator
if (!([Security.Principal.WindowsPrincipal][Security.Principal.WindowsIdentity]::GetCurrent()).IsInRole([Security.Principal.WindowsBuiltInRole] "Administrator")) { Start-Process powershell.exe "-NoProfile -ExecutionPolicy Bypass -File `"$PSCommandPath`"" -Verb RunAs; exit }

Set-ExecutionPolicy Unrestricted -Force

$DesktopPath = [Environment]::GetFolderPath("Desktop")
$pwCache="$DesktopPath/cached_password"
# Asks for user input (password) so should be run early
read-host -AsSecureString | ConvertFrom-SecureString | Out-File "$pwCache"
# $cred=Get-Credential $env:UserDomain\$env:UserName
$cred = New-Object System.Management.Automation.PSCredential ($env:Username, $Password) 


# $scriptpath = $MyInvocation.MyCommand.Path
# $scriptdir = Split-Path $scriptpath
$scriptdir = $PSScriptRoot
cd $scriptdir
[Environment]::CurrentDirectory = $PWD

# Install chocolatey
# if(-not(powershell choco -v 2>&1 | out-null)){
  if(-not(Get-Command "choco" -ErrorAction SilentlyContinue)){
    # Check if admin
    if-not([Security.Principal.WindowsIdentity]::GetCurrent().Groups -contains 'S-1-5-32-544'){
      $env:ChocolateyInstall="$env:USERPROFILE\Applications\chocolatey"
    }
    Set-ExecutionPolicy Bypass -Scope Process -Force; iex ((New-Object System.Net.WebClient).DownloadString('https://chocolatey.org/install.ps1'))
    # Reload path.
    $env:Path = [System.Environment]::GetEnvironmentVariable("Path","Machine") + ";" + [System.Environment]::GetEnvironmentVariable("Path","User") 
}
choco feature enable -n=allowGlobalConfirmation

$scriptdir | out-file -filepath $env:APPDATA\dotfiles_win_setup_dir.txt

# Setup powershell
if (!(Test-Path $profile)) {
  New-Item -path $profile -type file –force
}
$powershellRCPath="$profile"
$sourceText=". ${scriptdir}..\..\shell\powershell\powershellrc.ps1"
function addTextIfAbsent{
    param($text, $file)
    if (!(Select-String -Path $file -Pattern $text -SimpleMatch -Quiet)) {
            Add-Content $file $text
    }
}
addTextIfAbsent $sourceText $powershellRCPath
# Trust plugin store to avoid having to manually confirm each plugin installation.
Set-PSRepository -Name PSGallery -InstallationPolicy Trusted

git-bash -c '$(cygpath -u "'"$scriptdir"'")/install.sh -l'

. "$scriptdir\boxstarter_setup.ps1"
