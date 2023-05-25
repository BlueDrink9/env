$env:USEPF = 1
$env:TERMCOLOR = 16

$git_usr_bin=$(Join-path `
        -path $(split-path $(get-command git.exe).Path -parent)`
        -childpath "..\usr\bin")
$Env:Path += [IO.Path]::PathSeparator + "$git_usr_bin"

$Shell = $Host.UI.RawUI
# $Shell.WindowTitle="SysadminGeek"
# $size = $Shell.WindowSize
# $size.width=70
# $size.height=25
# $Shell.WindowSize = $size

# $size = $Shell.BufferSize
# $size.width=70
# $size.height=5000
# $Shell.BufferSize = $size
