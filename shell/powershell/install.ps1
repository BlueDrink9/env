$scriptdir = $PSScriptRoot
if (!(Test-Path $profile)) {
  New-Item -path $profile -type file -force
}
$powershellRCPath="$profile"
$sourceText=". ${scriptdir}\powershellrc.ps1"
function addTextIfAbsent{
    param($text, $file)
    if (!(Select-String -Path $file -Pattern $text -SimpleMatch -Quiet)) {
            Add-Content $file $text
    }
}
addTextIfAbsent $sourceText $powershellRCPath
