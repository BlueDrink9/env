$git_usr_bin=$(Join-path `
        -path $(split-path $(get-command git.exe).Path -parent)`
        -childpath "..\usr\bin")
$Env:Path += [IO.Path]::PathSeparator + "$git_usr_bin"
if (Get-Command "nvim" -ErrorAction SilentlyContinue) {
    $env:VISUAL='nvim'
} elseif (Get-Command "gvim" -ErrorAction SilentlyContinue) {
    $env:VISUAL='gvim'
} elseif (Get-Command "vim" -ErrorAction SilentlyContinue) {
    $env:VISUAL='vim'
} elseif (Get-Command "codium" -ErrorAction SilentlyContinue) {
    $env:VISUAL='codium'
} elseif (Get-Command "code" -ErrorAction SilentlyContinue) {
    $env:VISUAL='code'
} elseif (Get-Command "notepad++" -ErrorAction SilentlyContinue) {
    $env:VISUAL='notepad++'
} else {
    $env:VISUAL='notepad'
}

if ($env:WT_SESSION){
    $env:COLORTERM='truecolor'
    $env:USENF='1'
} else {
    $env:USEPF = 1
    $env:TERMCOLOR = 16
}

if (Get-Command "direnv" -ErrorAction SilentlyContinue) {
    Invoke-Expression "$(direnv hook pwsh)"
}

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
