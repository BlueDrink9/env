# To be run from administrative shell
Set-ExecutionPolicy Unrestricted -Force

# Install chocolatey
Set-ExecutionPolicy Bypass -Scope Process -Force; iex ((New-Object System.Net.WebClient).DownloadString('https://chocolatey.org/install.ps1'))
choco feature enable -n=allowGlobalConfirmation

CINST Boxstarter -y
Import-Module Boxstarter.Chocolatey
$Boxstarter.RebootOk=$true
$Boxstarter.NoPassword=$false
$Boxstarter.AutoLogin=$true
$cred=Get-Credential domain\username
Install-BoxstarterPackage -PackageName ".\boxstarter script.ps1" -Credential $cred
Install-BoxstarterPackage -PackageName ".\packages.ps1" -Credential $cred
