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
    "vb",
    "vbs",
    "0",
    "vim",
    "py",
    "sh"
)
foreach ($ext in $plaintextExtensions)
{
    cmd /c "assoc .$ext=plaintext"
}

$plaintextEditMenuExtensions = @(
    "reg",
    "bat",
    "ahk",
    "py",
    "ps1"
)

foreach ($ext in $plaintextEditMenuExtensions)
{
    cmd /c "assoc .$ext=plaintextRunnable"
}

$vimbin = "$(where gvim.exe)"
if ([string]::IsNullOrEmpty($vimbin)) {
    $vimbin = 'C:\Program Files (x86)\Vim\vim80\gvim.exe'
}
cmd /c "ftype plaintext=$vimbin `"%1`""
cmd /c "ftype text=$vimbin `"%1`""
if (-not (test-path "HKCU:\Software\Classes\plaintextRunnable\Shell\Edit\command")) {
    New-Item -Path "HKCU:\Software\Classes\plaintextRunnable"
    New-Item -Path "HKCU:\Software\Classes\plaintextRunnable\Shell"
    New-Item -Path "HKCU:\Software\Classes\plaintextRunnable\Shell\Edit"
    New-Item -Path "HKCU:\Software\Classes\plaintextRunnable\Shell\Edit\command"
}
# Set-item alone sets default.
New-ItemProperty -Path "HKCU:\Software\Classes\plaintextRunnable\Shell\Edit\command" -Name "(Default)" -PropertyType String -Value "`"$vimbin`" `"%1`""
