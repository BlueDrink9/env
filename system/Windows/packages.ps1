$packages = @(
        "git",
        "spotify",
        "steam",
        "adobereader",
        "windirstat",
        "skype",
        "python",
        "python2",
        "paint.net",
        "audacity",
        "autohotkey",
        "clink",
        "conemu",
        "cmder",
        "vlc",
        "sysinternals",
        "vscode",
        "wsl",
        "opera",
        "firefox",
        "googlechrome",
        "vim",
        "7zip",
        "gitkraken",
        "tortoisegit",
        "git-credential-manager-for-windows",
        "git-credential-winstore",
        "spotify",
        "steam"
)
foreach ($package in $packages)
{
    cinst $update -y
}

# cinst Microsoft-Hyper-V-All -source windowsFeatures
