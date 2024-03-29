# Run this line manually beforehand to allow this script to run, which will
# elevate itself then handle the rest.
# Set-ExecutionPolicy -scope currentuser Unrestricted -Force
#
# To be run from administrative shell
##Requires -RunAsAdministrator
if (!([Security.Principal.WindowsPrincipal][Security.Principal.WindowsIdentity]::GetCurrent()).IsInRole([Security.Principal.WindowsBuiltInRole] "Administrator")) { Start-Process powershell.exe "-NoProfile -ExecutionPolicy Bypass -File `"$PSCommandPath`"" -Verb RunAs; exit }

Set-ExecutionPolicy Unrestricted -Force -Scope CurrentUser
Set-ExecutionPolicy Unrestricted -Force

$DesktopPath = [Environment]::GetFolderPath("Desktop")
$pwCache="$DesktopPath/cached_password"
# Asks for user input (password) so should be run early
$password = Read-Host "Enter user Password" -AsSecureString
write $password |  ConvertFrom-SecureString | Out-File "$pwCache"
# $cred=Get-Credential $env:UserDomain\$env:UserName
$cred = New-Object System.Management.Automation.PSCredential ($env:Username, $password) 


# $scriptpath = $MyInvocation.MyCommand.Path
# $scriptdir = Split-Path $scriptpath
$scriptdir = $PSScriptRoot
cd $scriptdir
[Environment]::CurrentDirectory = $PWD

$scriptdir | out-file -filepath $env:APPDATA\dotfiles_win_setup_dir.txt

pushd "${scriptdir}\..\..\shell\powershell"
. ".\install.ps1"
popd

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
Install-PackageProvider -Name NuGet -MinimumVersion 2.8.5.201 -Force
choco feature enable -n=allowGlobalConfirmation

# Trust plugin store to avoid having to manually confirm each plugin installation.
Set-PSRepository -Name PSGallery -InstallationPolicy Trusted

$scriptdir = $PSScriptRoot
# . "$scriptdir\boxstarter_setup.ps1"
