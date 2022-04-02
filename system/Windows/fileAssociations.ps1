# vim: foldmethod=marker:foldmarker={[},{]}

# Won't ever work. Gets gvim.bat in sys32.
# $editor = "$(Get-Command gvim).Path"

# Duplicated from settings, is that necessary?
function createNewRegKey($path){
    if(!(Test-Path $path)){
        # This deletes the contents of the key, which is why we test it exists first.
        New-Item $path -Force;
    }
}
function Set-RegKey($path, $name, $value, $Type="DWORD", $PropertyType="DWORD"){
    createNewRegKey($path)
    If ($Null -eq (Get-ItemProperty -Path $path -Name $name -ErrorAction SilentlyContinue)) {
        New-ItemProperty -Path $Path -Name $name -Value $Value -PropertyType $PropertyType -Force
    } Else {
        Set-ItemProperty -Path $Path -Name $name -Value $Value -Force
    }
}



if ([string]::IsNullOrEmpty($editor)) {
    $editor = 'C:\tools\vim\latest\gvim.exe'
}
cmd /c "ftype plaintext=$editor `"%1`""
cmd /c "ftype text=$editor `"%1`""

if ([string]::IsNullOrEmpty($mplayer)) {
    $mplayer = 'C:\Program Files\VideoLAN\VLC\vlc.exe'
}
cmd /c "ftype audio=$mplayer `"%1`""
cmd /c "ftype video=$mplayer `"%1`""

########################################
############### Text ################## {[}
########################################

# Files without extensions = text
cmd /c "assoc .=plaintext"

$plaintextExtensions = @(
    "ini",
    "cfg",
    "conf",
    "yml",
    "yaml",
    "pl",
    "rb",
    "txt",
    "asc",
    "text",
    "c",
    "h",
    "cpp",
    "hpp",
    "md",
    "tex",
    "markdown",
    "tex",
    "bib",
    "bibtex",
    "log",
    "utf8",
    "class",
    "java",
    "R",
    "rmd",
    "rnoweb",
    "lisp",
    "php",
    "json",
    # "xml",
    "0",
    "vim",
    "py",
    "sh"
)
foreach ($ext in $plaintextExtensions)
{
    cmd /c "assoc .$ext=plaintext"
}

# "reg",
# "bat",
# "ahk",
# "py",
# "ps1"
# "vb",
# "vbs",
# This is not the same as the extension. It is a separate key that the
# extension has as a filetype.
$plaintextEditMenuFTs = @(
        "regfile",
        "batfile",
        "AutoHotkeyScript",
        "Python.File",
        "Microsoft.PowerShellScript.1",
        "xmlfile",
        "JSFile",
        "VBSFile"
)

# PYthon by default only has 'edit with idle' context command.
if (-not (test-path "HKLM:\Software\Classes\Python.File\Shell\Edit\command")) {
    New-Item -Path "HKLM:\Software\Classes\Python.File\Shell\Edit"
    New-Item -Path "HKLM:\Software\Classes\Python.File\Shell\Edit\command"
}
# Set edit menu command.
foreach ($ft in $plaintextEditMenuFTs)
{
    Set-regkey -Path "HKLM:\Software\Classes\$ft\Shell\Edit\command" -Name "(Default)" -PropertyType String -Value "`"$editor`" `"%1`""
}
# {]}


########################################
############### Audio ################## {[}
########################################
$audioExtensions = @(
    "m4a",
    "avi",
    "mp3"
)
foreach ($ext in $audioExtensions)
{
    cmd /c "assoc .$ext=audio"
}
# {]}

########################################
############### Video ################## {[}
########################################
$videoExtensions = @(
    "m4v",
    "mp4"
)
foreach ($ext in $videoExtensions)
{
    cmd /c "assoc .$ext=video"
}
# {]}

