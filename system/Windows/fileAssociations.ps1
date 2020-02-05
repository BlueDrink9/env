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
    "markdown",
    "tex",
    "bib",
    "bibtex",
    "log",
    "utf8",
    "class",
    "java",
    "R",
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
        "VBSFile"
)

$vimbin = "$(where gvim.exe)"
if ([string]::IsNullOrEmpty($vimbin)) {
    $vimbin = 'C:\tools\vim\latest\gvim.exe'
}
cmd /c "ftype plaintext=$vimbin `"%1`""
cmd /c "ftype text=$vimbin `"%1`""

# PYthon by default only has 'edit with idle' context command.
if (-not (test-path "HKLM:\Software\Classes\Python.File\Shell\Edit\command")) {
    New-Item -Path "HKLM:\Software\Classes\Python.File\Shell\Edit"
    New-Item -Path "HKLM:\Software\Classes\Python.File\Shell\Edit\command"
}
# Set edit menu command.
foreach ($ft in $plaintextEditMenuFTs)
{
    New-ItemProperty -Path "HKLM:\Software\Classes\$ft\Shell\Edit\command" -Name "(Default)" -PropertyType String -Value "`"$vimbin`" `"%1`""
}
