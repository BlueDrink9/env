$localscriptdir = $PSScriptRoot
pushd $localscriptdir
[Environment]::CurrentDirectory = $PWD

# Make registry drive for HKEY_CLASSES_ROOT hive
New-PSDrive -PSProvider registry -Root HKEY_CLASSES_ROOT -Name HKCR

function ChocoUpgradeAndRefresh($package){
    # Need to specify cache to avoid recursive dir issue. See
    # https://github.com/chocolatey/boxstarter/issues/241
    $chocoCache="$env:userprofile\AppData\Local\ChocoCache"
    choco upgrade --cacheLocation "$chocoCache" $package -y
    refreshenv
}

function InstallPackagesFromFile($packageFile, $installFunc=$function:ChocoUpgradeAndRefresh){
    # Read file skipping # comments and blank lines.
    Get-Content -Path "$packageFile"  | Where { $_ -notmatch '^#.*' -and $_ -notmatch '^\s*$' } | foreach-object {
        # Preserve spaces to pass options. Not working.
        $package=$_
        $installFunc.invoke($package)
  #       Invoke-Command -ScriptBlock {
  # param([string[]]$words) $words -join ' '
  # } -ArgumentList $array
    }
}

function AddToPath($path){
    $oldpath = (Get-ItemProperty -Path 'Registry::HKEY_LOCAL_MACHINE\System\CurrentControlSet\Control\Session Manager\Environment' -Name PATH).path
    [Environment]::SetEnvironmentVariable("Path", "$env:Path;$path", "Machine")
    $newpath = "$oldpath;$path"
    Set-ItemProperty -Path 'Registry::HKEY_LOCAL_MACHINE\System\CurrentControlSet\Control\Session Manager\Environment' -Name PATH -Value $newPath
}

Get-ChildItem "packages/" -filter "*.conf" | foreach-object {
    InstallPackagesFromFile("packages/$_")
}
# Need to try and open the winget page to install it properly prior to win 11
start-process ms-windows-store:
start-sleep -s 5
start-process "https://www.microsoft.com/en-nz/p/app-installer/9nblggh4nns1"
# winget
# app-installer
start-sleep -s 15
Get-ChildItem "packages/" -filter "*.cfg" | foreach-object {
    InstallPackagesFromFile "packages/$_"  winget
}


# # Downloads ubuntu for use
# # ========================
Enable-WindowsOptionalFeature -Online -FeatureName Microsoft-Windows-Subsystem-Linux
# Needed for WSL2
Enable-WindowsOptionalFeature -Online -FeatureName VirtualMachinePlatform
# # This didn't work, seemed to just cause problems.
# # ubuntu
# # pushd $env:TMP
# # $distro = "wsl-ubuntu-1804"
# # curl.exe -L -o "$distro.appx" "https://aka.ms/$distro"
# # Add-AppxPackage ".\$distro.appx"
# # popd

# colortool.exe /b solarized_dark
# Choco colortool is a bit funny
colortool.exe -b solarized_dark.itermcolors
# cinst Microsoft-Hyper-V-All -source windowsFeatures

Install-Module -Force OpenSSHUtils -Scope AllUsers

# Create symlink folder for latest version of vim
# ===============================================
# This is where chocolatey keeps vim now.
$ToolsVimDir="C:\tools\vim"
$linkname="${ToolsVimDir}\latest"

# Define helper as per linked answer. https://stackoverflow.com/a/32818070
$VerNumsToNatural = { [regex]::Replace($_, '\d+', { $args[0].Value.PadLeft(20) }) }
# Get dirlist, Sort with helper and check the output is natural result
$latestVimDir = gci $ToolsVimDir | sort $VerNumsToNatural -Descending | select -First 1

$source=$latestVimDir.fullname
New-Item -ItemType SymbolicLink -Path $linkname -Target $source

# For DOOM emacs
[System.Environment]::SetEnvironmentVariable("EMACS_SERVER_FILE", "${env:HOME}\.emacs.d\server\server", [System.EnvironmentVariableTarget]::User)

# Use latest path for 'edit with vim' context menu command.
$RegKeyPath = "HKCR:\*\shell\Vim\Command"
Set-Item -Path "$RegKeyPath" -Value "`"$linkname\gvim.exe`" `"%1`""


# Add miniconda to path
# $oldpath = (Get-ItemProperty -Path 'Registry::HKEY_LOCAL_MACHINE\System\CurrentControlSet\Control\Session Manager\Environment' -Name PATH).path
# $condapaths = ";c:\ProgramData\miniconda3;c:\ProgramData\miniconda3\scripts"
# [Environment]::SetEnvironmentVariable("Path", $env:Path + $condapaths, "Machine")
# $newpath = "$oldpath$condapaths"
# Set-ItemProperty -Path 'Registry::HKEY_LOCAL_MACHINE\System\CurrentControlSet\Control\Session Manager\Environment' -Name PATH -Value $newPath
# Oh never mind, just use the windows store version. It integrates best with
# path anyway, and hopefully will work best with other tools.
# Opens windows store if python not installed.
python

popd
