# Run this line manually beforehand to allow this script to run, which will
# elevate itself then handle the rest.
# Set-ExecutionPolicy -scope currentuser Unrestricted -Force
#
# To be run from administrative shell
##Requires -RunAsAdministrator
if (!([Security.Principal.WindowsPrincipal][Security.Principal.WindowsIdentity]::GetCurrent()).IsInRole([Security.Principal.WindowsBuiltInRole] "Administrator")) { Start-Process powershell.exe "-NoProfile -ExecutionPolicy Bypass -File `"$PSCommandPath`"" -Verb RunAs; exit }

Set-ExecutionPolicy Unrestricted -Force

# $scriptpath = $MyInvocation.MyCommand.Path
# $scriptdir = Split-Path $scriptpath
$scriptdir = $PSScriptRoot
cd $scriptdir
[Environment]::CurrentDirectory = $PWD

# Install chocolatey
# if(-not(powershell choco -v 2>&1 | out-null)){
if(-not(Get-Command "choco" -ErrorAction SilentlyContinue)){
    Set-ExecutionPolicy Bypass -Scope Process -Force; iex ((New-Object System.Net.WebClient).DownloadString('https://chocolatey.org/install.ps1'))
    # Reload path.
    $env:Path = [System.Environment]::GetEnvironmentVariable("Path","Machine") + ";" + [System.Environment]::GetEnvironmentVariable("Path","User") 

}
if(-not(Get-Command "boxstarter" -ErrorAction SilentlyContinue)){
    choco install Boxstarter -y
}
choco feature enable -n=allowGlobalConfirmation

$scriptdir | out-file -filepath $env:APPDATA\dotfiles_win_setup_dir.txt
Import-Module Boxstarter.Chocolatey
$Boxstarter.RebootOk=$true
$Boxstarter.NoPassword=$true
$Boxstarter.AutoLogin=$true
$cred=Get-Credential domain\username
Install-BoxstarterPackage -PackageName "$scriptdir\boxstarter-main.ps1" -Credential $cred


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

