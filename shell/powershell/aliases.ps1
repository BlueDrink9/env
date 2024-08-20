# Aliases to programs with arguments must be created as functions. Consider just using functions
# instead$args.
Set-Alias which where.exe
Set-Alias g git
function gs { git status $args}
function gc { git commit $args}
# Remove-Item -force alias:gc
function gca { git commit --all $args}
function gup { git commit --amend --no-edit --all $args}
function gupa { git commit --amend --no-edit --all $args}
function gupe { git commit --amend $args}

# Allows pip to auto-update, and ensures you use the right version of pip for the current environment
function pip {python -m pip $args}
function venv {python -m venv $args}
function venvc {python -m venv venv $args}
function venva {. venv/Scripts/activate.ps1 $args}
function gdiff { git diff --ignore-space-change --color-words --no-index $args}
Set-Alias e $env:VISUAL
function liteEdit {nvim --cmd "let g:liteMode=1" $args}
Set-Alias le liteEdit
function ide {nvim --cmd "let g:ideMode=1" $args}
Set-Alias time measure-command
Set-Alias fopen explorer.exe
function envupd { git -C "$DOTFILES_DIR" pull $args}
# function sudo { elevate.exe -k $args}  # k = persisten$argst
Set-Alias PlugUpdate Update-Module
Set-Alias plugu Update-Module

# ln -h source/target linkname
function ln($source, $linkname, $s, $j, $h){
    if ($j){
        New-Item -ItemType junction -Path $linkname -Target $source
    } elseif ($h){
        New-Item -ItemType hardlink -Path $linkname -Target $source
    } else { #if ($s){
        New-Item -ItemType SymbolicLink -Path $linkname -Target $source
    }
}

function rg { &$(get-command rg.exe) --smart-case $args}
set-alias komo komorebic
function pym { python -m $args}
set-alias py python

function packs { winget search $args}
function packi { winget install --silent $args}
function pack? { winget show $args}
function packrm { winget uninstall $args}
