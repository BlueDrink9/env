if(-not(Get-Command "boxstarter" -ErrorAction SilentlyContinue)){
    choco install Boxstarter -y
}
Import-Module Boxstarter.Chocolatey
$env:PSModulePath = [System.Environment]::GetEnvironmentVariable("PSModulePath","Machine")
$Boxstarter.RebootOk=$true
$Boxstarter.NoPassword=$true
$Boxstarter.AutoLogin=$true
Install-BoxstarterPackage -PackageName "$scriptdir\boxstarter-main.ps1" -Credential $cred

