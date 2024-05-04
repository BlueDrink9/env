$ErrorActionPreference = "Stop"
# Global variable to indicate whether the user can create symlinks
$CanCreateSymLinks = $false
$tempFile = New-TemporaryFile
try {
# Test creating a symbolic link to see if the user has permissions
    New-Item -ItemType SymbolicLink -Path "$env:TEMP\testlink.temp" -Target "$tempFile"
        $CanCreateSymLinks = $true
} catch {
# Check if user can elevate and self-elevate if required
    $currentUser = [Security.Principal.WindowsIdentity]::GetCurrent()
        $principle = New-Object Security.Principal.WindowsPrincipal($currentUser)
        $isAdmin = $principle.IsInRole([Security.Principal.WindowsBuiltInRole]::Administrator)
        if ($isAdmin) {
            $CommandLine = "-File `"$PSCommandPath`" $args"
                Start-Process -FilePath PowerShell.exe -Verb Runas -ArgumentList $CommandLine
                Exit
        }
}

# Function to create symbolic link if possible, or copy with replacement otherwise
function CreateLinkOrCopy($target, $destination) {
    if ($CanCreateSymLinks) {
        New-Item -ItemType SymbolicLink -Path $destination -Target $target
    } else {
        Copy-Item -Path $target -Destination $destination -Force
    }
}
# Loop through the different AppData paths for VS Code and VSCodium
$AppDataPaths = @(
    "${env:APPDATA}\Code\User",
    "${env:APPDATA}\VSCodium\User"
)

# Create symbolic links or copy files for each path
foreach ($AppDataPath in $AppDataPaths) {
    New-Item "$AppDataPath" -ItemType Directory -ea 0
    CreateLinkOrCopy "$PSScriptRoot\settings.json" "$AppDataPath\settings.json"
    CreateLinkOrCopy "$PSScriptRoot\keybindings.json" "$AppDataPath\keybindings.json"
    CreateLinkOrCopy "$PSScriptRoot\..\vim\runtimepath\snippets" "$AppDataPath\snippets"
}



# enable microsoft extension gallery
$product_json = @"
{
  "extensionsGallery": {
    "serviceUrl": "https://marketplace.visualstudio.com/_apis/public/gallery",
    "itemUrl": "https://marketplace.visualstudio.com/items",
    "cacheUrl": "https://vscode.blob.core.windows.net/gallery/index",
    "controlUrl": ""
  }
}
"@

Out-File -FilePath "${env:APPDATA}\VSCodium\product.json" -InputObject "$product_json"

Write-Host "Installing extensions"
$Extensions = Get-Content "$PSScriptRoot\extensions.txt"

$Binaries = @("code", "vscodium", "codium")
# Loop through the binaries list and remove the ones that are not available
$AvailableBinaries = @()
foreach ($Binary in $Binaries) {
    if (Get-Command $Binary -ErrorAction SilentlyContinue) {
        $AvailableBinaries += $Binary
    }
}

function New-TemporaryDirectory {
    $parent = [System.IO.Path]::GetTempPath()
        [string] $name = [System.Guid]::NewGuid()
        New-Item -ItemType Directory -Path (Join-Path $parent $name)
}
$TempDir = New-TemporaryDirectory

# Install extensions in the available editors
foreach ($Binary in $AvailableBinaries) {
    $InstalledExtensions = & $Binary --list-extensions
    $ToInstall = $Extensions | Where-Object {$_ -notin $InstalledExtensions}

    Write-Host "Installing these in ${Binary}: $ToInstall"
    foreach ($ExtensionId in $ToInstall) {
        # Download as vsix. Does't work because there's no way to get the latest package version.
        # $Publisher, $ID = $ExtensionId -split "\."
        # $DownloadUrl = "https://marketplace.visualstudio.com/_apis/public/gallery/publishers/$Publisher/vsextensions/$ID/$version/vspackage"
        # $DownloadPath = Join-Path -Path $TempDir -ChildPath "$ExtensionId.vsix"
        # Invoke-WebRequest -Uri $DownloadUrl -OutFile $DownloadPath
        & $Binary --install-extension $ExtensionId

    }
    Write-Host "Extensions installation ended in $Binary."
}

if ($AvailableBinaries.Count -eq 0) {
    Write-Host "None of the specified editors ($($Binaries -join ', ')) were found."
}

