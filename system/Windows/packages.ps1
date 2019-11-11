# wox: Ignore dep so that it doesn't pull in the latest python3.
$packages = @(
        "git",
        "vim",
        "cloneapp",
        "lastpass",
        "choco-package-list-backup",
        "7zip",
        "desktopicons-winconfig --params '/Computer:YES /UserFiles:YES /RecycleBin:YES'",
        "desktopok",
        "googlechrome",
        "classic-shell",
        "winaero-tweaker",
        "windows-tweaker",
        "7-taskbar-tweaker",
        "spotify",
        "officeproplus2013",
        "steam",
        "goggalaxy",
        "uplay",
        "origin",
        "loot",
        "adobereader",
        "windirstat",
        "skype",
        "pip",
        "paint.net",
        "audacity",
        "autohotkey",
        "clink",
        "conemu",
        "cmder",
        "vlc",
        "sysinternals",
        "vscode",
        "opera",
        "firefox",
        "gitkraken",
        "tortoisegit",
        "git-credential-manager-for-windows",
        "git-credential-winstore",
        "spotify",
        "jre8",
        "javaruntime",
        "malwarebytes",
        "sumatrapdf",
        "inkscape",
        "gimp",
        "teamviewer",
        "itunes",
        "autoruns",
        "virtualbox",
        "procexp",
        "procmon",
        "chocolateygui",
        "vcredist2012",
        "vcredist140",
        "dotnetfx",
        "dotnet3.5",
        "directx",
        "irfanview",
        "calibre",
        "google-backup-and-sync",
        "qbittorrent",
        "pandoc",
        "everything",
        "wox --ignoredependencies",
        "sumatrapdf",
        "rufus",
        "yumi",
        "f.lux",
        "revouninstallerpro",
        "mp3tag",
        "speccy",
        "nirlauncher",
        "handbrake",
        "processhacker",
        "partitionwizard",
        "windowsrepair",
        "hashcheck",
        "teracopy",
        "sudo",
        "graphviz",
        "synctrayzor",
        "ccleaner",
        "recuva",
        "reshack",
        "onenote",
        "xming",
        "sandboxie.install",
        "ripgrep",
        "ctags",
        "zotero",
        "internet-download-manager",
        "wordweb-free",
        "stickies",
        "hunt-and-peck",
        "eithermouse",
        "draglock",
        "text-editor-anywhere",
        "workrave",
        "ifunbox",
        "veusz",
        "megasync",
        "testdisk",
        "antimicro",
        "fixwin",
        "combofix",
        "partitionmasterfree",
        "todobackup",
        "defaultprogramseditor",
        "gamesavemanager",
        "hamachi",
        "mp3directcut",
        "pinnacle-game-profiler",
        "shexview",
        "texlive",
        "wsltty",
        "miktex.install",
        "diskgenius",
        "droidexplorer",
        "disable-nvidia-telemetry",
        "sharpkeys",
        "ibackupbot",
        "setpoint",
        "logitechgaming",
        "unifying",
        "onedrive",
        "colortool",
        "joplin",
        "google-backup-and-sync",
        "onetastic"
)
        # This gets the opentype version, which looks awful/doesn't alias
        # properly on win. Manually get the ttf version.
        # "miniconda",
        # "miniconda3",
        # "SourceCodePro",
        # "wsl",
        # The versioning here is a bit off it seems. Use conda instead.
        # This also installs python3 as python, so you can't easily use both
        # python 2 and 3 with other programs, like vim.
        # "python",
        # "python2",
foreach ($package in $packages)
{
    # Need to specify cache to avoid recursive dir issue. See
    # https://github.com/chocolatey/boxstarter/issues/241
    choco upgrade --cacheLocation "$env:userprofile\AppData\Local\ChocoCache" $package -y
    refreshenv
}
# Downloads ubuntu for use
Get-WindowsOptionalFeature -Online -FeatureName Microsoft-Windows-Subsystem-Linux
Enable-WindowsOptionalFeature -Online -FeatureName Microsoft-Windows-Subsystem-Linux
# This didn't work, seemed to just cause problems.
# ubuntu
# pushd $env:TMP
# $distro = "wsl-ubuntu-1804"
# curl.exe -L -o "$distro.appx" "https://aka.ms/$distro"
# Add-AppxPackage ".\$distro.appx"
# popd

# colortool.exe /b solarized_dark
# Choco colortool is a bit funny
colortool.exe -b solarized_dark.itermcolors
# cinst Microsoft-Hyper-V-All -source windowsFeatures

Install-Module -Force OpenSSHUtils -Scope AllUsers

# Add miniconda to path
# $oldpath = (Get-ItemProperty -Path 'Registry::HKEY_LOCAL_MACHINE\System\CurrentControlSet\Control\Session Manager\Environment' -Name PATH).path
# $condapaths = ";c:\ProgramData\miniconda3;c:\ProgramData\miniconda3\scripts"
# [Environment]::SetEnvironmentVariable("Path", $env:Path + $condapaths, "Machine")
# $newpath = "$oldpath$condapaths"
# Set-ItemProperty -Path 'Registry::HKEY_LOCAL_MACHINE\System\CurrentControlSet\Control\Session Manager\Environment' -Name PATH -Value $newPath
# Oh never mind, just use the windows store version. It integrates best with
# path anyway, and hopefully will work best with other tools.
python
